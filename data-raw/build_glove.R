# data-raw/build_glove.R
# Reads 2014 and 2024 GloVe 300d text files, subsets to words present in
# lexis_wide, then keeps only words present in both releases. Saves named
# numeric matrices as data/glove2014.rda and data/glove2024.rda.

library(dplyr)
library(readr)

find_base_dir <- function() {
  env_dir <- Sys.getenv("LEXIS_BASE_DIR", unset = "")
  if (nzchar(env_dir)) return(normalizePath(env_dir, mustWork = TRUE))

  this_file <- tryCatch(
    normalizePath(sys.frame(1)$ofile, mustWork = FALSE),
    error = function(e) ""
  )
  file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (!nzchar(this_file) && length(file_arg)) {
    this_file <- sub("^--file=", "", file_arg[[1]])
  }
  candidates <- unique(c(getwd(), dirname(this_file), dirname(dirname(this_file))))
  candidates <- candidates[nzchar(candidates)]

  for (start in candidates) {
    current <- normalizePath(start, mustWork = FALSE)
    repeat {
      desc <- file.path(current, "DESCRIPTION")
      if (file.exists(desc) && any(grepl("^Package:\\s+lexis\\s*$", readLines(desc, warn = FALSE)))) {
        return(current)
      }
      parent <- dirname(current)
      if (identical(parent, current)) break
      current <- parent
    }
  }

  stop(
    "Could not locate the lexis package root. Set LEXIS_BASE_DIR to the repo path.",
    call. = FALSE
  )
}

read_glove_txt <- function(glove_txt, ndim, norms_vocab) {
  message("Streaming ", ndim, "d GloVe in chunks (low-memory mode)...")
  chunk_size <- as.integer(Sys.getenv("LEXIS_GLOVE_CHUNK_SIZE", unset = "50000"))
  if (is.na(chunk_size) || chunk_size < 1000L) chunk_size <- 50000L

  kept_words <- character(0)
  kept_vals  <- list()
  raw_vocab_n <- 0L
  chunk_i <- 0L

  chunk_cb <- readr::SideEffectChunkCallback$new(function(lines, pos) {
    chunk_i    <<- chunk_i + 1L
    raw_vocab_n <<- raw_vocab_n + length(lines)

    parts    <- strsplit(lines, " ", fixed = TRUE)
    words    <- vapply(parts, `[`, "", 1)
    keep_idx <- words %in% norms_vocab

    if (any(keep_idx)) {
      sel  <- parts[keep_idx]
      vals <- lapply(sel, function(x) as.numeric(x[seq(2, ndim + 1)]))
      kept_vals[[length(kept_vals) + 1L]] <<- do.call(rbind, vals)
      kept_words <<- c(kept_words, words[keep_idx])
    }

    if (chunk_i %% 5L == 0L) {
      message("  Processed ", raw_vocab_n, " rows; kept ", length(kept_words), " so far")
    }
  })

  readr::read_lines_chunked(
    file       = glove_txt,
    callback   = chunk_cb,
    chunk_size = chunk_size,
    progress   = TRUE
  )

  if (!length(kept_vals)) stop("No overlap found between norms vocabulary and GloVe rows.")

  mat_raw           <- do.call(rbind, kept_vals)
  rownames(mat_raw) <- kept_words
  colnames(mat_raw) <- paste0("V", seq_len(ndim))

  keep <- intersect(norms_vocab, rownames(mat_raw))
  mat  <- mat_raw[keep, , drop = FALSE]

  message("  Raw vocab:    ", raw_vocab_n)
  message("  Norms vocab:  ", length(norms_vocab))
  message("  Intersection: ", nrow(mat))
  message("  Coverage:     ", round(100 * nrow(mat) / length(norms_vocab), 1), "%")

  mat
}

base_dir  <- find_base_dir()
embed_dir <- file.path(base_dir, "datasets/xother/glove-embeddings")
build_dir <- file.path(base_dir, "data-raw/_build")
lexis_rds <- file.path(build_dir, "lexis_wide.rds")

if (!file.exists(lexis_rds)) {
  stop("data-raw/_build/lexis_wide.rds not found — run data-raw/build_lexis.R first.", call. = FALSE)
}

lexis_wide  <- readRDS(lexis_rds)
norms_vocab <- unique(tolower(lexis_wide$word))

glove_sources <- list(
  glove2014 = list(
    path = file.path(embed_dir, "2014", "glove.6B.300d.txt"),
    ndim = 300L
  ),
  glove2024 = list(
    path = file.path(embed_dir, "2024", "wiki_giga_2024_300_MFT20_vectors_seed_2024_alpha_0.75_eta_0.05_combined.txt"),
    ndim = 300L
  )
)

mats <- list()
for (obj_name in names(glove_sources)) {
  src <- glove_sources[[obj_name]]
  if (!file.exists(src$path)) {
    stop(obj_name, " file not found: ", src$path, call. = FALSE)
  }
  message("\n=== Building ", obj_name, " ===")
  mats[[obj_name]] <- read_glove_txt(src$path, src$ndim, norms_vocab)
}

common_words <- Reduce(intersect, lapply(mats, rownames))
if (!length(common_words)) {
  stop("No words overlap between GloVe 2014 and 2024 after lexis filtering.", call. = FALSE)
}

message(
  "\n=== Restricting to 2014/2024 intersection (", length(common_words), " words) ==="
)
for (obj_name in names(mats)) {
  before <- nrow(mats[[obj_name]])
  assign(obj_name, mats[[obj_name]][common_words, , drop = FALSE])
  out <- file.path(base_dir, "data", paste0(obj_name, ".rda"))
  save(list = obj_name, file = out, compress = "xz")
  message(
    "Saved ", obj_name, " (", nrow(get(obj_name)), " words x ", ncol(get(obj_name)),
    " dims; dropped ", before - length(common_words), ")"
  )
}
