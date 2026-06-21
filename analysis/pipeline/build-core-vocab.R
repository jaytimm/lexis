#!/usr/bin/env Rscript

suppressPackageStartupMessages({
  if (!requireNamespace("pacman", quietly = TRUE)) {
    install.packages("pacman", repos = "https://cloud.r-project.org")
  }
  pacman::p_load(dplyr, readr, tidyr, purrr)
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

add_flags <- function(df, glove_words) {
  df |>
    mutate(
      has_glove      = word %in% glove_words,
      has_freq       = !is.na(prevalence_freq_zipf),
      has_prevalence = !is.na(prevalence_score),
      has_conc       = !is.na(concreteness),
      has_poly       = !is.na(wn_n_synsets) & wn_n_synsets > 0,
      has_aoa        = !is.na(aoa),
      has_lancaster  = if_all(
        c(
          lancaster_auditory, lancaster_gustatory, lancaster_haptic,
          lancaster_interoceptive, lancaster_olfactory, lancaster_visual,
          lancaster_foot_leg, lancaster_hand_arm, lancaster_head,
          lancaster_mouth, lancaster_torso
        ),
        ~ !is.na(.x)
      ),
      has_vad        = !is.na(valence) & !is.na(arousal) & !is.na(dominance),
      has_iconicity  = !is.na(iconicity),
      has_socialness = !is.na(socialness)
    )
}

build_strata <- function(df) {
  df |>
    mutate(
      conc_q    = dplyr::ntile(concreteness, 5),
      freq_band = dplyr::ntile(prevalence_freq_zipf, 3),
      poly_band = case_when(
        wn_n_synsets == 1  ~ "1",
        wn_n_synsets <= 3  ~ "2_3",
        wn_n_synsets <= 9  ~ "4_9",
        TRUE               ~ "10_plus"
      ),
      strata_id = paste(conc_q, freq_band, poly_band, sep = "_")
    )
}

compute_holdout_allocations <- function(cell_counts, holdout_n) {
  alloc <- cell_counts |>
    mutate(
      holdout_raw       = n / sum(n) * holdout_n,
      holdout_take      = floor(holdout_raw),
      holdout_remainder = holdout_raw - holdout_take
    )

  shortfall <- holdout_n - sum(alloc$holdout_take)
  if (shortfall > 0) {
    add_idx <- order(alloc$holdout_remainder, decreasing = TRUE)[seq_len(shortfall)]
    alloc$holdout_take[add_idx] <- alloc$holdout_take[add_idx] + 1L
  }

  alloc |>
    mutate(holdout_take = pmin(holdout_take, n)) |>
    select(strata_id, n, holdout_take)
}

assign_split <- function(df, holdout_prop) {
  holdout_n <- as.integer(round(nrow(df) * holdout_prop))

  cell_counts <- df |>
    count(strata_id, conc_q, freq_band, poly_band) |>
    arrange(conc_q, freq_band, poly_band)

  holdout_alloc <- compute_holdout_allocations(cell_counts, holdout_n)

  df |>
    inner_join(select(holdout_alloc, strata_id, holdout_take), by = "strata_id") |>
    {\(d) bind_rows(lapply(split(d, d$strata_id), function(cell_df) {
      holdout_idx <- sample.int(nrow(cell_df), size = cell_df$holdout_take[[1]], replace = FALSE)
      cell_df$split <- "train"
      cell_df$split[holdout_idx] <- "holdout"
      cell_df
    }))}() |>
    select(-holdout_take)
}

build_senses_long <- function(core_words, wordnet_synsets_path, wordset_dict_path) {
  parts <- list()

  if (file.exists(wordnet_synsets_path)) {
    wn_senses <- read_csv(wordnet_synsets_path, col_types = cols(), show_col_types = FALSE) |>
      filter(word %in% core_words) |>
      transmute(
        word,
        source     = "wordnet",
        pos        = pos,
        sense_rank = as.integer(sense_rank),
        definition = definition,
        example    = if_else(nzchar(examples), examples, NA_character_),
        synonyms   = NA_character_
      )
    parts[["wordnet"]] <- wn_senses
  } else {
    message("wordnet_synsets.csv not found; omitting WordNet senses.")
  }

  if (file.exists(wordset_dict_path)) {
    ws_senses <- readRDS(wordset_dict_path) |>
      filter(tolower(trimws(word)) %in% core_words) |>
      mutate(word = tolower(trimws(word))) |>
      group_by(word) |>
      mutate(sense_rank = row_number()) |>
      ungroup() |>
      transmute(
        word,
        source     = "wordset",
        pos        = speech_part,
        sense_rank,
        definition = def,
        example,
        synonyms
      )
    parts[["wordset"]] <- ws_senses
  } else {
    message("wordset_dict.rds not found; omitting Wordset senses.")
  }

  bind_rows(parts) |>
    arrange(word, source, sense_rank)
}

to_long <- function(df, dims) {
  sd_cols <- intersect(paste0(dims, "_sd"), names(df))

  mean_long <- df |>
    select(word, lemma, split, conc_q, freq_band, poly_band,
           prevalence_freq_zipf, wn_n_synsets, all_of(dims)) |>
    pivot_longer(cols = all_of(dims), names_to = "dimension", values_to = "score")

  sd_long <- df |>
    select(word, all_of(sd_cols)) |>
    pivot_longer(cols = all_of(sd_cols), names_to = "dimension", values_to = "sd") |>
    mutate(dimension = sub("_sd$", "", dimension))

  mean_long |>
    left_join(sd_long, by = c("word", "dimension")) |>
    arrange(split, dimension, word)
}

# ── Paths ──────────────────────────────────────────────────────────────────────

base_dir <- find_base_dir()
build_dir <- file.path(base_dir, "data-raw", "_build")
out_dir   <- file.path(base_dir, "analysis", "output")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

wide_path <- file.path(build_dir, "lexis_wide.rds")
long_path <- file.path(build_dir, "lexis_long.rds")
glove_path <- file.path(base_dir, "data", "glove2024.rda")

if (!file.exists(wide_path))  stop("Missing: ", wide_path,  call. = FALSE)
if (!file.exists(long_path))  stop("Missing: ", long_path,  call. = FALSE)
if (!file.exists(glove_path)) stop("Missing: ", glove_path, call. = FALSE)

lexis_wide <- readRDS(wide_path)
load(glove_path)

lexis_sd_wide <- readRDS(long_path) |>
  filter(!is.na(sd)) |>
  select(word, dimension, sd) |>
  pivot_wider(names_from = dimension, values_from = sd,
              names_glue = "{dimension}_sd", values_fn = mean)

lexis_wide <- lexis_wide |>
  left_join(lexis_sd_wide, by = "word")


required_cols <- c(
  "word", "lemma", "aoa", "concreteness", "socialness",
  "iconicity", "valence", "arousal", "dominance",
  "prevalence_freq_zipf", "prevalence_score", "wn_n_synsets",
  "lancaster_auditory", "lancaster_gustatory", "lancaster_haptic",
  "lancaster_interoceptive", "lancaster_olfactory", "lancaster_visual",
  "lancaster_foot_leg", "lancaster_hand_arm", "lancaster_head",
  "lancaster_mouth", "lancaster_torso"
)

missing_cols <- setdiff(required_cols, names(lexis_wide))
if (length(missing_cols)) {
  stop("Missing required columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

# ── Build eligible pool ────────────────────────────────────────────────────────

set.seed(20260511)

all_flags <- c(
  "has_glove", "has_freq", "has_prevalence", "has_conc", "has_poly",
  "has_aoa", "has_lancaster", "has_vad", "has_iconicity", "has_socialness"
)

glove_words <- rownames(glove2024)

eligible_pool <- lexis_wide |>
  add_flags(glove_words) |>
  filter(if_all(all_of(all_flags), identity)) |>
  build_strata() |>
  distinct(word, .keep_all = TRUE)

message("Eligible pool: ", nrow(eligible_pool), " words")

# ── Assign train/holdout split within strata ───────────────────────────────────

core <- assign_split(eligible_pool, holdout_prop = 0.25)

strata_summary <- core |>
  count(split, conc_q, freq_band, poly_band, name = "n") |>
  arrange(split, conc_q, freq_band, poly_band)

# ── Build outputs ──────────────────────────────────────────────────────────────

long_dims <- c(
  "aoa", "concreteness", "socialness", "iconicity", "prevalence_score",
  "valence", "arousal", "dominance",
  "lancaster_auditory", "lancaster_gustatory", "lancaster_haptic",
  "lancaster_interoceptive", "lancaster_olfactory", "lancaster_visual",
  "lancaster_foot_leg", "lancaster_hand_arm", "lancaster_head",
  "lancaster_mouth", "lancaster_torso"
)

keep_wide_cols <- c(
  "word", "lemma", "split",
  "conc_q", "freq_band", "poly_band", "strata_id",
  "concreteness", "concreteness_sd",
  "prevalence_freq_zipf", "prevalence_score",
  "wn_n_synsets", "wn_n_noun", "wn_n_verb", "wn_n_adj", "wn_n_adv",
  "aoa", "aoa_sd",
  "socialness", "socialness_sd",
  "iconicity", "iconicity_sd",
  "valence", "valence_sd",
  "arousal", "arousal_sd",
  "dominance", "dominance_sd",
  "lancaster_auditory",      "lancaster_auditory_sd",
  "lancaster_gustatory",     "lancaster_gustatory_sd",
  "lancaster_haptic",        "lancaster_haptic_sd",
  "lancaster_interoceptive", "lancaster_interoceptive_sd",
  "lancaster_olfactory",     "lancaster_olfactory_sd",
  "lancaster_visual",        "lancaster_visual_sd",
  "lancaster_foot_leg",      "lancaster_foot_leg_sd",
  "lancaster_hand_arm",      "lancaster_hand_arm_sd",
  "lancaster_head",          "lancaster_head_sd",
  "lancaster_mouth",         "lancaster_mouth_sd",
  "lancaster_torso",         "lancaster_torso_sd"
)

core_wide <- core |>
  arrange(split, conc_q, freq_band, poly_band, word) |>
  select(all_of(keep_wide_cols)) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

core_long <- to_long(core, long_dims) |>
  mutate(across(where(is.numeric), ~ round(.x, 4)))

wordnet_synsets_path <- file.path(build_dir, "wordnet_synsets.csv")
wordset_dict_path    <- file.path(base_dir, "datasets/xother/wordset-dictionary/wordset_dict.rds")

core_senses <- build_senses_long(
  core_words           = unique(core_wide$word),
  wordnet_synsets_path = wordnet_synsets_path,
  wordset_dict_path    = wordset_dict_path
)

# ── Write outputs ──────────────────────────────────────────────────────────────

write_csv(core_wide,      file.path(out_dir, "core_vocab_wide.csv"))
write_csv(core_long,      file.path(out_dir, "core_vocab_long.csv"))
write_csv(strata_summary, file.path(out_dir, "core_vocab_strata_summary.csv"))
write_csv(core_senses,    file.path(out_dir, "core_vocab_senses_long.csv"))

message("Wrote:")
message("  core_vocab_wide.csv          : ", nrow(core_wide), " words")
message("  core_vocab_long.csv          : ", nrow(core_long), " rows")
message("  core_vocab_strata_summary.csv: ", nrow(strata_summary), " cells")
message("  core_vocab_senses_long.csv   : ", nrow(core_senses), " rows (",
        n_distinct(core_senses$word), " words, ",
        n_distinct(core_senses$source), " sources)")
message("\nSplit: ",
        sum(core_wide$split == "train"), " train / ",
        sum(core_wide$split == "holdout"), " holdout")
