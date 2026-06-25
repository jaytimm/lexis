#!/usr/bin/env Rscript
# build_app_data.R
# Regenerates the inlined data blocks of app/lexis-explorer.html from the
# installed lexis package, restricted to the pinned sample (app/sample_words.csv,
# the paper1 train split). Rewrites the <script id="meta-data"> and
# <script id="word-data"> blocks in place. Run after rebuilding the package.
#
#   Rscript data-raw/build_app_data.R

suppressPackageStartupMessages({
  if (!requireNamespace("lexis", quietly = TRUE))
    stop("Install lexis first: remotes::install_github('jaytimm/lexis')", call. = FALSE)
  if (!requireNamespace("jsonlite", quietly = TRUE))
    stop("jsonlite is required.", call. = FALSE)
  library(jsonlite)
})

app_dir   <- "app"
html_path <- file.path(app_dir, "lexis-explorer.html")
meta_path <- file.path(app_dir, "lexis_meta.json")          # curation source
samp_path <- file.path(app_dir, "sample_words.csv")
for (p in c(html_path, meta_path, samp_path))
  if (!file.exists(p)) stop("Missing: ", p, call. = FALSE)

# ── sample + package data ────────────────────────────────────────────────────
sample_words <- readLines(samp_path)[-1]                    # drop "word" header
wide <- lexis::lexis_wide
samp <- wide[match(sample_words, wide$word), , drop = FALSE]
samp <- samp[!is.na(samp$word), , drop = FALSE]
message("Sample: ", nrow(samp), " words (of ", length(sample_words), " requested)")

# ── meta: carry curated label/group/invert/min/max forward, fix schema ───────
meta <- jsonlite::fromJSON(meta_path)
meta <- meta[meta$key != "gender_femininity", ]            # gender norm removed
rename <- c(freq_zipf_us = "subtlex_us_zipf", wf_zipf = "wordfreq_en_zipf")
labels <- c(subtlex_us_zipf = "Frequency (SUBTLEX-US)",
            wordfreq_en_zipf = "Frequency (wordfreq)")
for (old in names(rename)) {
  i <- which(meta$key == old)
  if (length(i)) {
    meta$key[i]    <- rename[[old]]
    meta$source[i] <- rename[[old]]
    meta$label[i]  <- labels[[rename[[old]]]]
  }
}

# keep only dimensions that exist as columns in the package data
meta <- meta[meta$key %in% names(samp), ]
dim_keys <- meta$key

# refresh coverage (n) to within-sample counts
meta$n <- vapply(dim_keys, function(k) sum(!is.na(samp[[k]])), integer(1))

meta_json <- jsonlite::toJSON(meta, dataframe = "rows", na = "null", auto_unbox = TRUE)

# ── word-data: {w, l, ...non-null dims}, rounded, nulls omitted ──────────────
round_dim <- function(k, v) if (grepl("^wn_n_", k)) v else round(v, 2)
rows <- lapply(seq_len(nrow(samp)), function(i) {
  obj <- list(w = samp$word[i])
  lem <- samp$lemma[i]
  if (!is.na(lem) && lem != samp$word[i]) obj$l <- lem
  for (k in dim_keys) {
    v <- samp[[k]][i]
    if (!is.na(v)) obj[[k]] <- round_dim(k, v)
  }
  obj
})
word_json <- jsonlite::toJSON(rows, auto_unbox = TRUE, digits = 4)

# ── inject into the HTML in place (replace the two single-line blocks) ───────
html  <- readLines(html_path, warn = FALSE)
inject <- function(html, id, json) {
  i <- grep(paste0('id="', id, '"'), html, fixed = TRUE)
  if (length(i) != 1) stop("Expected exactly one '", id, "' block; found ", length(i))
  html[i] <- paste0('<script type="application/json" id="', id, '">', json, '</script>')
  html
}
html <- inject(html, "meta-data", meta_json)
html <- inject(html, "word-data", word_json)
writeLines(html, html_path, useBytes = TRUE)

message("Injected ", nrow(meta), " dimensions and ", length(rows),
        " words into ", html_path)
message("  dims: ", paste(dim_keys, collapse = ", "))
