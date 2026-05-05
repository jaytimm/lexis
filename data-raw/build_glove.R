# data-raw/build_glove.R
# Reads the 2024 GloVe text files (50d and 300d), subsets to words present in
# lexis_wide, and saves named numeric matrices as data/glove50.rda and
# data/glove300.rda.

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
    file      = glove_txt,
    callback  = chunk_cb,
    chunk_size = chunk_size,
    progress  = TRUE
  )

  if (!length(kept_vals)) stop("No overlap found between norms vocabulary and GloVe rows.")

  mat_raw          <- do.call(rbind, kept_vals)
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

txt50  <- file.path(embed_dir, "wiki_giga_2024_50_MFT20_vectors_seed_123_alpha_0.75_eta_0.075_combined.txt")
txt300 <- file.path(embed_dir, "wiki_giga_2024_300_MFT20_vectors_seed_2024_alpha_0.75_eta_0.05_combined.txt")

if (!file.exists(txt50))  stop("50d file not found: ",  txt50)
if (!file.exists(txt300)) stop("300d file not found: ", txt300)

glove50  <- read_glove_txt(txt50,  50L,  norms_vocab)
glove300 <- read_glove_txt(txt300, 300L, norms_vocab)

save(glove50,  file = file.path(base_dir, "data/glove50.rda"),  compress = "xz")
save(glove300, file = file.path(base_dir, "data/glove300.rda"), compress = "xz")

message("Saved glove50  (", nrow(glove50),  " words x 50 dims)")
message("Saved glove300 (", nrow(glove300), " words x 300 dims)")
