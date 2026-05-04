# build_wordset.R
# Parses the wordset-dictionary JSON files into two flat dataframes:
#
#   wordset_dict  — one row per meaning (definition), all fields preserved
#   wordset_index — one row per word: word, pos (unique parts of speech), n_defs
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

base_dir <- dirname(normalizePath(sys.frame(1)$ofile, mustWork = FALSE))
if (!nzchar(base_dir) || base_dir == ".") {
  base_dir <- "/home/jtimm/Dropbox/working-papers/psycho-data"
}

json_dir <- file.path(base_dir, "xother/wordset-dictionary/jsons")
json_files <- list.files(json_dir, pattern = "\\.json$", full.names = TRUE)

# ─────────────────────────────────────────────────────────────────────────────
# Parse all JSON files into a single flat tibble (one row per meaning)
# ─────────────────────────────────────────────────────────────────────────────

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
wordset_dict <- map_dfr(json_files, parse_file) |>
  mutate(word = trimws(word))

message("  Rows (meanings): ", nrow(wordset_dict))
message("  Words:           ", n_distinct(wordset_dict$word))

# ─────────────────────────────────────────────────────────────────────────────
# Summary index: one row per word
# pos = unique speech_part values, alphabetically sorted, semicolon-separated
# n_defs = total number of meanings
# ─────────────────────────────────────────────────────────────────────────────

wordset_index <- wordset_dict |>
  group_by(word, pos = speech_part) |>
  summarise(n_defs = n(), .groups = "drop") |>
  arrange(word, pos)

message("  Index rows:      ", nrow(wordset_index))

# ─────────────────────────────────────────────────────────────────────────────
# Save
# ─────────────────────────────────────────────────────────────────────────────

saveRDS(wordset_dict,  file.path(base_dir, "xother/wordset-dictionary/wordset_dict.rds"))
saveRDS(wordset_index, file.path(base_dir, "xother/wordset-dictionary/wordset_index.rds"))

write_csv(wordset_dict,  file.path(base_dir, "xother/wordset-dictionary/wordset_dict.csv"))
write_csv(wordset_index, file.path(base_dir, "xother/wordset-dictionary/wordset_index.csv"))

message("Done.")
