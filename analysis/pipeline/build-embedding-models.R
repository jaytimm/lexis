#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman", repos = "https://cloud.r-project.org")
  }
  pacman::p_load(dplyr, readr, glmnet)
})

find_base_dir <- function() {
  current <- normalizePath(getwd(), mustWork = TRUE)
  repeat {
    desc <- file.path(current, "DESCRIPTION")
    if (file.exists(desc)) return(current)
    parent <- dirname(current)
    if (identical(parent, current)) break
    current <- parent
  }
  stop("Could not locate package root from working directory.", call. = FALSE)
}

metrics <- function(observed, predicted) {
  tibble::tibble(
    rmse = sqrt(mean((observed - predicted)^2)),
    mae  = mean(abs(observed - predicted)),
    cor  = suppressWarnings(cor(observed, predicted))
  )
}

base_dir <- find_base_dir()
out_dir  <- file.path(base_dir, "analysis", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

samples_path    <- file.path(out_dir, "core_vocab_long.csv")
glove2014_path  <- file.path(base_dir, "data", "glove2014.rda")
glove2024_path  <- file.path(base_dir, "data", "glove2024.rda")
bge_csv_path    <- file.path(out_dir, "bge_large_embeddings.csv")

if (!file.exists(samples_path))   stop("Missing: ", samples_path,   call. = FALSE)
if (!file.exists(glove2014_path)) stop("Missing: ", glove2014_path, call. = FALSE)
if (!file.exists(glove2024_path)) stop("Missing: ", glove2024_path, call. = FALSE)

norm_samples <- readr::read_csv(samples_path, show_col_types = FALSE)

load_rda <- function(path, obj) {
  load(path)
  get(obj)
}

embeddings <- list(
  glove2014 = load_rda(glove2014_path, "glove2014"),
  glove2024 = load_rda(glove2024_path, "glove2024")
)

if (file.exists(bge_csv_path)) {
  bge_raw <- readr::read_csv(bge_csv_path, show_col_types = FALSE)
  bge_mat <- as.matrix(bge_raw[, -1])
  rownames(bge_mat) <- bge_raw$word
  embeddings[["bge_large"]] <- bge_mat
} else {
  message(
    "bge_large_embeddings.csv not found — skipping. ",
    "Run analysis/build-bge-embeddings.py first."
  )
}

set.seed(20260505)

dimensions       <- sort(unique(norm_samples$dimension))
prediction_rows  <- list()
diagnostic_rows  <- list()

for (embed_name in names(embeddings)) {
  emb <- embeddings[[embed_name]]

  for (dimension in dimensions) {
    key <- paste0(embed_name, "__", dimension)
    message("Modeling [", embed_name, "] ", dimension)

    sample_df <- norm_samples |>
      filter(dimension == !!dimension, word %in% rownames(emb)) |>
      arrange(split, word)

    train_df   <- sample_df |> filter(split == "train")
    holdout_df <- sample_df |> filter(split == "holdout")

    x_train   <- emb[train_df$word,   , drop = FALSE]
    y_train   <- train_df$score
    x_holdout <- emb[holdout_df$word, , drop = FALSE]
    y_holdout <- holdout_df$score

    cv_fit <- cv.glmnet(
      x           = x_train,
      y           = y_train,
      alpha       = 0,
      nfolds      = 5,
      type.measure = "mse"
    )

    holdout_pred <- as.numeric(predict(cv_fit, newx = x_holdout, s = "lambda.min"))
    train_pred   <- as.numeric(predict(cv_fit, newx = x_train,   s = "lambda.min"))

    prediction_rows[[key]] <- bind_rows(
      train_df |>
        transmute(word, lemma, dimension, split, observed = score, predicted = train_pred),
      holdout_df |>
        transmute(word, lemma, dimension, split, observed = score, predicted = holdout_pred)
    ) |> mutate(embedding = embed_name, .after = dimension)

    holdout_metrics <- metrics(y_holdout, holdout_pred)

    diagnostic_rows[[key]] <- holdout_metrics |>
      mutate(
        dimension  = dimension,
        embedding  = embed_name,
        train_n    = nrow(train_df),
        holdout_n  = nrow(holdout_df),
        lambda_min = round(cv_fit$lambda.min, 5),
        .before    = 1
      )
  }
}

predictions <- bind_rows(prediction_rows)
diagnostics <- bind_rows(diagnostic_rows) |>
  mutate(
    rmse = round(rmse, 3),
    mae  = round(mae,  3),
    cor  = round(cor,  3)
  ) |>
  arrange(dimension, embedding)

readr::write_csv(predictions, file.path(out_dir, "embedding_model_predictions.csv"))
readr::write_csv(diagnostics, file.path(out_dir, "embedding_model_diagnostics.csv"))

message("Wrote:")
message("  ", file.path(out_dir, "embedding_model_predictions.csv"))
message("  ", file.path(out_dir, "embedding_model_diagnostics.csv"))
