# build_wordset.R
# Parses the wordset-dictionary JSON files into two flat dataframes:
#
#   wordset_dict  — one row per meaning (definition), all fields preserved
#   wordset_index — one row per word with:
#                   n_defs (total definitions), n_pos (distinct POS count),
#                   pos (semicolon-separated distinct POS)
#
# Source: xother/wordset-dictionary/jsons/*.json
# Each JSON is a named list: word_string → {word, wordset_id, meanings: [...]}
# Each meaning has: id, def, speech_part, and optionally example, synonyms, labels.
# 'labels' is a list of {name, is_dialect, parent: {name, is_dialect}}; collapsed
# to a semicolon-separated string of label names (parent first if present).
# '-example' is a rare alternate field name for example — treated identically.

library(dplyr)
library(purrr)
library(jsonlite)
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

json_dir <- file.path(base_dir, "datasets/xother/wordset-dictionary/jsons")
json_files <- list.files(json_dir, pattern = "\\.json$", full.names = TRUE)
if (length(json_files) == 0) {
  stop("No Wordset JSON files found under: ", json_dir, call. = FALSE)
}

dict_rds  <- file.path(base_dir, "datasets/xother/wordset-dictionary/wordset_dict.rds")
index_rds <- file.path(base_dir, "datasets/xother/wordset-dictionary/wordset_index.rds")
dict_csv  <- file.path(base_dir, "datasets/xother/wordset-dictionary/wordset_dict.csv")
index_csv <- file.path(base_dir, "datasets/xother/wordset-dictionary/wordset_index.csv")

force_rebuild <- identical(tolower(Sys.getenv("LEXIS_FORCE_REBUILD", unset = "false")), "true") ||
  "--force" %in% commandArgs(trailingOnly = TRUE)
skip_wordset <- FALSE

outputs_fresh <- function(outputs, inputs) {
  all(file.exists(outputs)) &&
    min(file.info(outputs)$mtime) >= max(file.info(inputs)$mtime)
}

if (!force_rebuild && outputs_fresh(c(dict_rds, index_rds), json_files)) {
  message("Wordset dictionary already built; skipping JSON parse.")
  message("  dict : ", dict_rds)
  message("  index: ", index_rds)
  skip_wordset <- TRUE
}

# ─────────────────────────────────────────────────────────────────────────────
# Parse all JSON files into a single flat tibble (one row per meaning)
# ─────────────────────────────────────────────────────────────────────────────
if (!skip_wordset) {

collapse_labels <- function(labels) {
  if (is.null(labels) || length(labels) == 0) return(NA_character_)
  parts <- map_chr(labels, function(lb) {
    nm <- lb[["name"]] %||% ""
    parent_nm <- lb[["parent"]][["name"]] %||% ""
    if (nzchar(parent_nm)) paste0(parent_nm, "/", nm) else nm
  })
  paste(parts, collapse = "; ")
}

parse_file <- function(path) {
  entries <- read_json(path)
  map_dfr(entries, function(entry) {
    w   <- entry[["word"]]
    wid <- entry[["wordset_id"]]
    map_dfr(entry[["meanings"]], function(m) {
      tibble(
        word        = w,
        wordset_id  = wid,
        meaning_id  = m[["id"]]  %||% NA_character_,
        speech_part = m[["speech_part"]] %||% NA_character_,
        def         = m[["def"]] %||% NA_character_,
        example     = m[["example"]] %||% m[["-example"]] %||% NA_character_,
        synonyms    = {
          s <- m[["synonyms"]]
          if (is.null(s) || length(s) == 0) NA_character_
          else paste(unlist(s), collapse = "; ")
        },
        labels      = collapse_labels(m[["labels"]])
      )
    })
  })
}

message("Parsing ", length(json_files), " JSON files...")
wordset_dict <- purrr::imap_dfr(json_files, function(path, i) {
  message("  [", i, "/", length(json_files), "] ", basename(path))
  parse_file(path)
}) |>
  mutate(word = trimws(word))

message("  Rows (meanings): ", nrow(wordset_dict))
message("  Words:           ", n_distinct(wordset_dict$word))

# ─────────────────────────────────────────────────────────────────────────────
# Summary index: one row per word
# pos   = unique speech_part values, alphabetically sorted, semicolon-separated
# n_defs = total number of meanings
# n_pos  = number of unique POS values
# ─────────────────────────────────────────────────────────────────────────────

wordset_index <- wordset_dict |>
  mutate(
    word = tolower(trimws(word)),
    speech_part = tolower(trimws(speech_part))
  ) |>
  filter(!is.na(word), word != "") |>
  group_by(word) |>
  summarise(
    n_defs = n(),
    n_pos = n_distinct(speech_part[!is.na(speech_part) & nzchar(speech_part)]),
    pos = {
      vals <- sort(unique(speech_part[!is.na(speech_part) & nzchar(speech_part)]))
      if (length(vals) == 0) NA_character_ else paste(vals, collapse = ";")
    },
    .groups = "drop"
  ) |>
  arrange(word)

message("  Index rows:      ", nrow(wordset_index))

# ─────────────────────────────────────────────────────────────────────────────
# Save
# ─────────────────────────────────────────────────────────────────────────────

saveRDS(wordset_dict,  dict_rds)
saveRDS(wordset_index, index_rds)

write_csv(wordset_dict,  dict_csv)
write_csv(wordset_index, index_csv)

message("Done.")
}
