# data-raw/build_glove.R
# Reads the raw GloVe 50d text file, subsets to words present in lexis_wide,
# and saves a named numeric matrix as data/glove50.rda.
#
# Prerequisites:
#   - data-raw/build_lexis.R must have been run (data-raw/_build/lexis_wide.rds must exist)
#   - xother/glove-embeddings/glove.6B.50d.txt must be present
#
# The raw file is ~164 MB (400k words × 50 dims). The subset saved to data/
# is typically ~30-50 MB compressed, small enough for GitHub.
#
# To load the full 400k vocabulary (e.g., for neighbor search beyond the norms
# vocabulary), use lex_load_glove() which reads the raw file directly.

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

base_dir <- find_base_dir()
glove_txt <- file.path(base_dir, "xother/glove-embeddings/glove.6B.50d.txt")
out_path  <- file.path(base_dir, "data/glove50.rda")
build_dir <- file.path(base_dir, "data-raw/_build")
lexis_rds <- file.path(build_dir, "lexis_wide.rds")

if (!file.exists(glove_txt)) stop("GloVe text file not found: ", glove_txt)
if (!file.exists(lexis_rds)) {
  stop(
    "data-raw/_build/lexis_wide.rds not found — run data-raw/build_lexis.R first.",
    call. = FALSE
  )
}

lexis_wide <- readRDS(lexis_rds)
norms_vocab <- unique(tolower(lexis_wide$word))

message("Streaming GloVe 50d in chunks (low-memory mode)...")
chunk_size <- as.integer(Sys.getenv("LEXIS_GLOVE_CHUNK_SIZE", unset = "50000"))
if (is.na(chunk_size) || chunk_size < 1000) chunk_size <- 50000L

kept_words <- character(0)
kept_vals  <- list()
raw_vocab_n <- 0L
chunk_i <- 0L

chunk_cb <- readr::SideEffectChunkCallback$new(function(lines, pos) {
  chunk_i <<- chunk_i + 1L
  raw_vocab_n <<- raw_vocab_n + length(lines)

  parts <- strsplit(lines, " ", fixed = TRUE)
  words <- vapply(parts, `[`, "", 1)
  keep_idx <- words %in% norms_vocab

  if (any(keep_idx)) {
    sel <- parts[keep_idx]
    vals <- lapply(sel, function(x) as.numeric(x[2:51]))
    kept_vals[[length(kept_vals) + 1L]] <<- do.call(rbind, vals)
    kept_words <<- c(kept_words, words[keep_idx])
  }

  if (chunk_i %% 5L == 0L) {
    message("  Processed ", raw_vocab_n, " rows; kept ", length(kept_words), " so far")
  }
})

readr::read_lines_chunked(
  file = glove_txt,
  callback = chunk_cb,
  chunk_size = chunk_size,
  progress = TRUE
)

if (!length(kept_vals)) {
  stop("No overlap found between norms vocabulary and GloVe rows.", call. = FALSE)
}

mat_raw <- do.call(rbind, kept_vals)
rownames(mat_raw) <- kept_words
colnames(mat_raw) <- paste0("V", seq_len(50))

# Preserve lexis_wide order and remove duplicate tokens if present.
keep <- intersect(norms_vocab, rownames(mat_raw))
glove50 <- mat_raw[keep, , drop = FALSE]

message("  Raw vocab:        ", raw_vocab_n)
message("  Norms vocab:      ", length(norms_vocab))
message("  Intersection:     ", nrow(glove50))
message("  Coverage:         ",
        round(100 * nrow(glove50) / length(norms_vocab), 1), "%")

save(glove50, file = out_path, compress = "xz")
message("Saved to: ", out_path)
message("  Object size: ",
        format(utils::object.size(glove50), units = "MB"))
