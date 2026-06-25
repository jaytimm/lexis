# build_lexis.R
# Compiles English psycholinguistic norming datasets into a unified resource.
# Produces:
#   (a) lexis_long  — tidy long format: word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max
#   (b) lexis_wide  — one row per word, one column per dataset-dimension
#   (c) lexis_meta  — metadata: dimension, dataset, original_col, scale, citation
#
# ─── JUDGMENT CALLS ───────────────────────────────────────────────────────────
# BOI:
#   The file uses CR (not CRLF) line endings — read with read.csv() not read_csv().
#   Columns 5-6 (unnamed blank, "Colour key:") are annotation artifacts; dropped.
#   Items where > 15% of raters did not know the word are RETAINED as-is (annotated
#   in original file but not in a machine-readable column here). Filter on low N if needed.
#
# Valence-Arousal-Dominance (Warriner et al.):
#   Only full-sample summary columns (*.Sum) go into the long format.
#   Subgroup means by sex (M/F), age (Y=young/O=old), education (L/H) are retained
#   in the wide format only.
#   First column of the CSV is an unnamed integer row index — dropped.
#
# COMPOSITION:
#   lexis_long holds rater-averaged human rating norms ONLY — every dimension
#   carries per-item sd and n_ratings. Datasets lacking per-item reliability
#   (imageability, sensory experience, verbs-in-space) are excluded entirely.
#   Behavioural/corpus-derived measures are NOT norms and live in lexis_wide as
#   covariates only: lexdec RTs (lexdec_rt, lexdec_naming_rt), SUBTLEX-US Zipf
#   frequency (freq_zipf_us), wordfreq Zipf (wf_zipf), and WordNet sense counts
#   (wn_*). lexis_datasets carries per-study participant provenance.
#
# Gender (Roberts & Utych, 2019):
#   Scale 1–7 where higher = more feminine (1 = masculine, 7 = feminine).
#   'valence', 'arousal', 'dom' columns present in the XLSX but entirely NA — dropped.
#   'mean-a' is the combined-sex overall mean, used as primary dimension.
#
# Prevalence (Brysbaert et al., 2019):
#   Only FreqZipfUS is retained, as the freq_zipf_us covariate in wide. The
#   Pknown and Prevalence scores are dropped (not rating norms).
#
# Lexical Decision / English Lexicon Project (Balota et al., 2007):
#   I_Mean_RT (lexdec_rt) and I_NMG_Mean_RT (lexdec_naming_rt), in milliseconds,
#   retained as wide covariates. Source z-scores/accuracy are redundant — dropped.
#
# General:
#   All 'word' columns are lowercased and whitespace-trimmed for joining.
#   Duplicate (word, dimension) pairs are collapsed by mean in the wide format — rare
#   but possible if source files contain near-duplicate entries.
# ──────────────────────────────────────────────────────────────────────────────

library(dplyr)
library(tidyr)
library(readr)
library(readxl)
library(purrr)
library(textstem)

if (!exists("find_base_dir", mode = "function")) {
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
}

base_dir <- find_base_dir()

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1: READ & HARMONIZE EACH DATASET
# ─────────────────────────────────────────────────────────────────────────────

## 1a. Age of Acquisition — Kuperman et al. (2012) ────────────────────────────
# Columns: Word, OccurTotal (n lists), OccurNum (n raters), Rating.Mean, Rating.SD, Frequency
# Scale: years (~1–25). Frequency = SUBTLEX-US log frequency (retained in wide).
aoa_raw <- read_csv(
  file.path(base_dir, "datasets/aoa/Kuperman_et_al_2012.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

aoa <- aoa_raw |>
  rename(word = Word, mean = Rating.Mean, sd = Rating.SD, n_ratings = OccurNum) |>
  mutate(dataset = "aoa", dimension = "aoa", scale_min = 1, scale_max = 25) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1b. Body-Object Interaction — Pexman et al. (2019) ─────────────────────────
# File uses CR-only line endings; read.csv() handles this; read_csv() does not.
# Columns 5-6 are artifacts from Excel formatting — dropped.
boi_raw <- read.csv(
  file.path(base_dir, "datasets/boi/13428_2018_1171_MOESM1_ESM.csv"),
  header = TRUE, stringsAsFactors = FALSE, strip.white = TRUE
)

boi <- boi_raw |>
  select(Word, N, Mean, SD) |>
  rename(word = Word, n_ratings = N, mean = Mean, sd = SD) |>
  mutate(
    word      = trimws(word),
    mean      = as.numeric(mean),
    sd        = as.numeric(sd),
    n_ratings = as.integer(n_ratings),
    dataset   = "boi", dimension = "boi",
    scale_min = 1, scale_max = 7
  ) |>
  filter(!is.na(word), word != "") |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1c. Concreteness — Brysbaert et al. (2014) ─────────────────────────────────
# Columns: Word, Bigram, Conc.M, Conc.SD, Unknown (n unknowns), Total (n raters),
#          Percent_known, SUBTLEX (frequency). Scale: 1 (abstract) to 5 (concrete).
conc_raw <- read_csv(
  file.path(base_dir, "datasets/concreteness/Brysbaert_et_al_2014.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

conc <- conc_raw |>
  rename(word = Word, mean = Conc.M, sd = Conc.SD, n_ratings = Total) |>
  mutate(dataset = "concreteness", dimension = "concreteness",
         scale_min = 1, scale_max = 5) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1d. Gender — Roberts & Utych (2019) ────────────────────────────────────────
# Scale: 1 (masculine) to 7 (feminine). 'mean-a' = combined-sex overall mean.
# FLAGGED: 'valence', 'arousal', 'dom' columns are entirely NA — dropped.
gender_raw <- read_excel(
  file.path(base_dir, "datasets/gender/appendix_-_word_rating_file.xlsx")
)

gender <- gender_raw |>
  rename(word = Word, mean = `mean-a`, sd = `std.dev-a`, n_ratings = `n-all`) |>
  mutate(dataset = "gender", dimension = "gender_femininity",
         scale_min = 1, scale_max = 7) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1e. Humor — Engelthaler & Hills (2018) ─────────────────────────────────────
# Scale: 1 (not at all funny) to 5 (very funny).
humor_raw <- read_csv(
  file.path(base_dir, "datasets/humor/humor_dataset.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

humor <- humor_raw |>
  rename(n_ratings = n) |>
  mutate(dataset = "humor", dimension = "humor",
         scale_min = 1, scale_max = 5) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1g. Lancaster Sensorimotor Norms — Lynott et al. (2020) ────────────────────
# 11 perceptual modalities + 11 action modalities, each with .mean and .SD.
# Scale: 0 (not at all) to 5 (greatly). N per item not in this file.
# Composite summaries (Max_strength, Minkowski3, Exclusivity, Dominant) kept in wide only.
lanc_raw <- read_csv(
  file.path(base_dir, "datasets/lancaster/Lancaster_sensorimotor_norms_for_39707_words.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

# Perceptual modalities draw raters from the perceptual-strength component;
# effector/action modalities from the action-strength component. The source
# reports no per-item rater count, so the modality-level N_known (number of
# raters who knew the word in that component) is used as n_ratings.
lanc_perceptual <- c("Auditory", "Gustatory", "Haptic", "Interoceptive",
                     "Olfactory", "Visual")
lanc_action     <- c("Foot_leg", "Hand_arm", "Head", "Mouth", "Torso")
lanc_modalities <- c(lanc_perceptual, lanc_action)

lanc_long <- map_dfr(lanc_modalities, function(mod) {
  n_col <- if (mod %in% lanc_action) "N_known.action" else "N_known.perceptual"
  lanc_raw |>
    select(Word, all_of(paste0(mod, ".mean")), all_of(paste0(mod, ".SD")),
           all_of(n_col)) |>
    rename(word      = Word,
           mean      = all_of(paste0(mod, ".mean")),
           sd        = all_of(paste0(mod, ".SD")),
           n_ratings = all_of(n_col)) |>
    mutate(
      dataset   = "lancaster",
      dimension = paste0("lancaster_", tolower(mod)),
      n_ratings = as.integer(round(n_ratings)),
      scale_min = 0, scale_max = 5
    )
})

## 1h. English Lexicon Project — Balota et al. (2007) ─────────────────────────
# Behavioural latencies, joined to lexis_wide as covariates (not rating norms).
# I_Mean_RT: lexical decision RT (ms); I_NMG_Mean_RT: speeded naming RT (ms).
lex_raw <- read_csv(
  file.path(base_dir, "datasets/lexdec/Balota_et_al_2007.csv"),
  col_types = cols(),
  na = c("NA", ""),
  show_col_types = FALSE
)

## 1i. Word Prevalence — Brysbaert et al. (2019) ───────────────────────────────
# Only FreqZipfUS (SUBTLEX-US Zipf frequency) is retained, as the freq_zipf_us
# covariate in lexis_wide. The Pknown and Prevalence scores are not included.
prev_raw <- read_excel(
  file.path(base_dir, "datasets/prevalence/13428_2018_1077_MOESM2_ESM.xlsx")
)

## 1k. Socialness — Diveica et al. (2023) ─────────────────────────────────────
# Scale: 1 (not at all social) to 7 (highly social).
soc_raw <- read_csv(
  file.path(base_dir, "datasets/socialness/Ratings_sum.stat.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

soc <- soc_raw |>
  rename(word = Word, mean = Mean, sd = SD, n_ratings = N) |>
  mutate(dataset = "socialness", dimension = "socialness",
         scale_min = 1, scale_max = 7) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1l. Iconicity — Winter et al. (2023) ──────────────────────────────────────
# Scale: 1–7 (arbitrary to iconic). SD available. prop_known retained as supp.
iconicity_raw <- read_csv(
  file.path(base_dir, "datasets/iconicity/iconicity_ratings_cleaned.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

iconicity <- iconicity_raw |>
  rename(word = word, mean = rating, sd = rating_sd, n_ratings = n_ratings) |>
  mutate(dataset = "iconicity", dimension = "iconicity",
         scale_min = 1, scale_max = 7) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1m. Valence, Arousal, Dominance — Warriner et al. (2013) ───────────────────
# Scale: 1–9 (Self-Assessment Manikin). Full-sample summary (*.Sum) only.
# First column is an unnamed integer row index from original CSV — dropped automatically
# because it does not match any select() target.
vad_raw <- read_csv(
  file.path(base_dir, "datasets/valence-arousal-dominance/BRM-emot-submit.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

vad_valence <- vad_raw |>
  select(Word, V.Mean.Sum, V.SD.Sum, V.Rat.Sum) |>
  rename(word = Word, mean = V.Mean.Sum, sd = V.SD.Sum, n_ratings = V.Rat.Sum) |>
  mutate(dataset = "warriner_vad", dimension = "valence",
         scale_min = 1, scale_max = 9)

vad_arousal <- vad_raw |>
  select(Word, A.Mean.Sum, A.SD.Sum, A.Rat.Sum) |>
  rename(word = Word, mean = A.Mean.Sum, sd = A.SD.Sum, n_ratings = A.Rat.Sum) |>
  mutate(dataset = "warriner_vad", dimension = "arousal",
         scale_min = 1, scale_max = 9)

vad_dominance <- vad_raw |>
  select(Word, D.Mean.Sum, D.SD.Sum, D.Rat.Sum) |>
  rename(word = Word, mean = D.Mean.Sum, sd = D.SD.Sum, n_ratings = D.Rat.Sum) |>
  mutate(dataset = "warriner_vad", dimension = "dominance",
         scale_min = 1, scale_max = 9)

## 1n. Glasgow Norms — Scott et al. (2019) ────────────────────────────────────
# Nine dimensions, each with M/SD/N, from one UK rater pool (University of
# Glasgow). The file has a two-row header (dimension group, then M/SD/N), so we
# skip both rows and assign column names explicitly. Dimensions are prefixed
# `glasgow_` so they form distinct wide columns rather than silently averaging
# with the US-rated valence/concreteness/etc. norms. Scales: valence, arousal,
# dominance are 1–9; the rest are 1–7. Note Glasgow AoA is a 1–7 band rating
# (not years, unlike the Kuperman `aoa` norm), and Glasgow gender runs
# 1 = feminine to 7 = masculine (the reverse of `gender_femininity`).
glasgow_cols <- c(
  "word", "length",
  "arou_m","arou_sd","arou_n", "val_m","val_sd","val_n", "dom_m","dom_sd","dom_n",
  "cnc_m","cnc_sd","cnc_n", "imag_m","imag_sd","imag_n", "fam_m","fam_sd","fam_n",
  "aoa_m","aoa_sd","aoa_n", "size_m","size_sd","size_n", "gend_m","gend_sd","gend_n"
)
glasgow_raw <- read_csv(
  file.path(base_dir, "datasets/glasgow/13428_2018_1099_MOESM2_ESM.csv"),
  skip = 2, col_names = glasgow_cols, col_types = cols(), show_col_types = FALSE
)

glasgow_dims <- tribble(
  ~dim,           ~stem,  ~smin, ~smax,
  "valence",      "val",  1,     9,
  "arousal",      "arou", 1,     9,
  "dominance",    "dom",  1,     9,
  "concreteness", "cnc",  1,     7,
  "imageability", "imag", 1,     7,
  "familiarity",  "fam",  1,     7,
  "aoa",          "aoa",  1,     7,
  "size",         "size", 1,     7,
  "gender",       "gend", 1,     7
)

glasgow_long <- purrr::pmap_dfr(glasgow_dims, function(dim, stem, smin, smax) {
  glasgow_raw |>
    transmute(
      word      = tolower(trimws(word)),
      dataset   = "glasgow",
      dimension = paste0("glasgow_", dim),
      mean      = .data[[paste0(stem, "_m")]],
      sd        = .data[[paste0(stem, "_sd")]],
      n_ratings = as.integer(round(.data[[paste0(stem, "_n")]])),
      scale_min = smin,
      scale_max = smax
    )
})

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2: LONG-FORMAT TIDY DATAFRAME
# ─────────────────────────────────────────────────────────────────────────────

# lexis_long holds rater-averaged human rating norms only (each carries per-item
# sd and n_ratings). Behavioural/corpus-derived measures (lexdec RTs, frequency,
# WordNet counts) are joined to lexis_wide as covariates in Section 3, not here.
lexis_long <- bind_rows(
  aoa,
  boi,
  conc,
  gender,
  humor,
  iconicity,
  lanc_long |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  soc,
  vad_valence   |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  vad_arousal   |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  vad_dominance |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  glasgow_long  |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)
) |>
  mutate(
    word      = tolower(trimws(word)),
    mean      = as.numeric(mean),
    sd        = as.numeric(sd),
    n_ratings = as.integer(n_ratings)
  ) |>
  filter(!is.na(word), word != "", !grepl(" ", word))

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 3: WIDE-FORMAT DATAFRAME
# ─────────────────────────────────────────────────────────────────────────────

# 3a. Core wide from long (mean only)
lexis_wide <- lexis_long |>
  select(word, dimension, mean) |>
  pivot_wider(
    names_from  = dimension,
    values_from = mean,
    values_fn   = mean  # resolves rare true duplicates by averaging
  )

# 3b. Non-norm covariate columns joined to wide (not part of lexis_long)

# Behavioural latencies from the English Lexicon Project (raw RTs only; the
# z-score and accuracy columns are redundant with the RTs and are not retained).
lexdec_supp <- lex_raw |>
  transmute(word             = tolower(trimws(Word)),
            lexdec_rt        = I_Mean_RT,
            lexdec_naming_rt = I_NMG_Mean_RT)

# SUBTLEX-US Zipf frequency from the Brysbaert et al. (2019) prevalence release.
freq_supp <- prev_raw |>
  transmute(word         = tolower(trimws(Word)),
            freq_zipf_us = FreqZipfUS)

collapse_supp <- function(x) {
  x |>
    group_by(word) |>
    summarise(
      across(
        everything(),
        function(col) {
          if (is.numeric(col)) {
            out <- mean(col, na.rm = TRUE)
            if (is.nan(out)) NA_real_ else out
          } else {
            vals <- sort(unique(stats::na.omit(col)))
            if (length(vals) == 0) NA_character_ else paste(vals, collapse = ";")
          }
        }
      ),
      .groups = "drop"
    )
}

lexdec_supp <- collapse_supp(lexdec_supp)
freq_supp   <- collapse_supp(freq_supp)

# Join covariate columns
lexis_wide <- lexis_wide |>
  left_join(lexdec_supp, by = "word") |>
  left_join(freq_supp,   by = "word")

# WordNet sense counts — optional; requires build_wordnet.py to have run first.
wordnet_counts_path <- file.path(base_dir, "data-raw/_build/wordnet_defs.csv")
if (file.exists(wordnet_counts_path)) {
  wordnet_counts <- read_csv(
    wordnet_counts_path,
    col_types = cols(.default = col_guess()),
    show_col_types = FALSE
  ) |>
    select(word, wn_n_synsets, wn_n_noun, wn_n_verb, wn_n_adj, wn_n_adv) |>
    mutate(word = tolower(trimws(word)))
  lexis_wide <- lexis_wide |>
    left_join(wordnet_counts, by = "word")
  message("WordNet counts joined: ", nrow(wordnet_counts), " words")
} else {
  message("WordNet counts not found (run data-raw/build_wordnet.py); wn_* columns omitted.")
}

# wordfreq Zipf frequencies — optional; requires build_wordfreq.py to have run first.
wordfreq_path <- file.path(base_dir, "data-raw/_build/wordfreq.csv")
if (file.exists(wordfreq_path)) {
  wordfreq_vals <- read_csv(
    wordfreq_path,
    col_types = cols(.default = col_guess()),
    show_col_types = FALSE
  ) |>
    mutate(
      word    = tolower(trimws(word)),
      wf_zipf = if_else(wf_zipf == 0, NA_real_, wf_zipf)
    )
  lexis_wide <- lexis_wide |>
    left_join(wordfreq_vals, by = "word")
  message("wordfreq Zipf joined: ", sum(!is.na(wordfreq_vals$wf_zipf)), " words with non-zero frequency")
} else {
  message("wordfreq output not found (run data-raw/build_wordfreq.py); wf_zipf column omitted.")
}

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 4: METADATA DATAFRAME
# ─────────────────────────────────────────────────────────────────────────────

lexis_meta <- tribble(
  ~dimension,                  ~dataset,            ~original_col,            ~scale,                        ~sd_available,
  ~construct,
  ~instructions,
  ~citation,

  "aoa", "aoa", "Rating.Mean", "1–25 (years)", TRUE,
  "The estimated age (in years) at which a person first learned a word, defined as the age at which they would have understood it if someone had used it in front of them, even without having read, written, or spoken it themselves. Earlier acquisition is associated with faster lexical access and greater processing fluency.",
  "For each word, enter the age (in years) at which you think you learned the word. 'By learning a word, we mean the age at which you would have understood that word if somebody had used it in front of you, EVEN IF YOU DID NOT use, read, or write it at the time.' Enter 'x' if you do not know the word.",
  "Kuperman, Stadthagen-Gonzalez & Brysbaert (2012). Behav Res Methods, 44, 978–990.",

  "boi", "boi", "Mean", "1–7", TRUE,
  "Body-object interaction (BOI) measures the ease with which the human body can physically interact with the referent of a word. High-BOI words (e.g., boot, map) refer to objects that are readily manipulated with the hands or body; low-BOI words (e.g., moon, ocean) refer to entities that resist direct bodily contact. BOI predicts lexical decision and semantic decision times above and beyond concreteness and imageability.",
  "Participants rated each word on the question 'How easily can you interact with this object/concept using your hands or body?' on a scale from 1 (not at all easily) to 7 (very easily), with an 'I don't know this word' option available.",
  "Pexman, Muraki, Sidhu, Siakaluk & Yap (2019). Behav Res Methods, 51, 1134–1146.",

  "concreteness", "concreteness", "Conc.M", "1–5", TRUE,
  "Concreteness captures the degree to which a word's meaning refers to a perceptible, experience-based entity (concrete) versus an abstract concept known only through language and definition. Concrete words can be experienced directly through the five senses and motor actions; abstract words depend on language-based meaning.",
  "'Some words refer to things or actions in reality, which you can experience directly through one of the five senses. We call these words concrete. Other words refer to meanings that cannot be experienced directly but which we know because the meanings can be defined by other words. These are abstract words.' Participants rated each word from 1 (abstract, language-based) to 5 (concrete, experience-based); N indicated the word was unknown.",
  "Brysbaert, Warriner & Kuperman (2014). Front Psychol, 5, 1515.",

  "gender_femininity", "gender", "mean-a", "1–7 (1=masc, 7=fem)", TRUE,
  "The perceived gender connotation of a word, reflecting the degree to which it is associated with masculinity or femininity in American political and cultural discourse. Higher scores indicate greater perceived femininity; lower scores indicate greater perceived masculinity.",
  "Participants rated each word on a 7-point scale from 'very masculine' (1) to 'very feminine' (7), reflecting their perception of the gender associations of the word's meaning.",
  "Roberts & Utych (2019). Am Polit Res, 47, 1155–1173.",

  "humor", "humor", "mean", "1–5", TRUE,
  "The perceived funniness of individual English words, reflecting the degree to which a word is experienced as amusing, absurd, or otherwise associated with humorous thought and language. Humor ratings are largely independent of valence, arousal, and concreteness, suggesting humor is a distinct lexical dimension.",
  "'You will rate how you felt while reading each word. The rating scale ranges from 1 (humorless = not funny at all) to 5 (humorous = most funny)... you feel the word is amusing or likely to be associated with humorous thought or language (for example, it is absurd, amusing, hilarious, playful, silly, whimsical, or laughable)... make your ratings based on your first and immediate reaction as you read each word.'",
  "Engelthaler & Hills (2018). Behav Res Methods, 50, 1116–1124.",

  "lancaster_auditory", "lancaster", "Auditory.mean", "0–5", TRUE,
  "Strength of auditory perceptual experience associated with a word's meaning — the degree to which the concept is experienced through hearing.",
  "'To what extent do you experience [WORD] by hearing?' Rated 0 (not at all experienced through hearing) to 5 (greatly experienced through hearing). A 'Don't know the meaning of this word' checkbox was available. Order of modality scales was randomized per item list.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_gustatory", "lancaster", "Gustatory.mean", "0–5", TRUE,
  "Strength of gustatory (taste) perceptual experience associated with a word's meaning — the degree to which the concept is experienced through tasting.",
  "'To what extent do you experience [WORD] by tasting?' Rated 0 (not at all) to 5 (greatly). Modality order randomized; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_haptic", "lancaster", "Haptic.mean", "0–5", TRUE,
  "Strength of haptic (touch) perceptual experience associated with a word's meaning — the degree to which the concept is experienced through physical contact or feeling through touch.",
  "'To what extent do you experience [WORD] by feeling through touch?' Rated 0 (not at all) to 5 (greatly). Modality order randomized; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_interoceptive", "lancaster", "Interoceptive.mean", "0–5", TRUE,
  "Strength of interoceptive experience associated with a word's meaning — the degree to which the concept is experienced through internal bodily sensations (e.g., hunger, heartbeat, pain). Interoception is particularly important for emotion and abstract concepts.",
  "'To what extent do you experience [WORD] by sensations inside your body?' Rated 0 (not at all) to 5 (greatly). Modality order randomized; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_olfactory", "lancaster", "Olfactory.mean", "0–5", TRUE,
  "Strength of olfactory (smell) perceptual experience associated with a word's meaning — the degree to which the concept is experienced through smelling.",
  "'To what extent do you experience [WORD] by smelling?' Rated 0 (not at all) to 5 (greatly). Modality order randomized; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_visual", "lancaster", "Visual.mean", "0–5", TRUE,
  "Strength of visual perceptual experience associated with a word's meaning — the degree to which the concept is experienced through seeing.",
  "'To what extent do you experience [WORD] by seeing?' Rated 0 (not at all) to 5 (greatly). Modality order randomized; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_foot_leg", "lancaster", "Foot_leg.mean", "0–5", TRUE,
  "Strength of foot/leg action associated with a word's meaning — the degree to which performing an action related to the concept involves the foot or leg effector.",
  "'To what extent do you experience [WORD] by performing an action with the Foot/leg?' Rated 0 (not at all) to 5 (greatly). Each effector was accompanied by a body avatar image. Effector order randomized; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_hand_arm", "lancaster", "Hand_arm.mean", "0–5", TRUE,
  "Strength of hand/arm action associated with a word's meaning — the degree to which performing an action related to the concept involves the hand or arm effector.",
  "'To what extent do you experience [WORD] by performing an action with the Hand/arm?' Rated 0 (not at all) to 5 (greatly). Effector order randomized; body avatar image provided; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_head", "lancaster", "Head.mean", "0–5", TRUE,
  "Strength of head action (excluding mouth) associated with a word's meaning — the degree to which performing an action related to the concept involves the head effector.",
  "'To what extent do you experience [WORD] by performing an action with the Head excluding mouth?' Rated 0 (not at all) to 5 (greatly). Effector order randomized; body avatar image provided; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_mouth", "lancaster", "Mouth.mean", "0–5", TRUE,
  "Strength of mouth/throat action associated with a word's meaning — the degree to which performing an action related to the concept involves the mouth or throat effector (e.g., speaking, eating, singing).",
  "'To what extent do you experience [WORD] by performing an action with the Mouth/throat?' Rated 0 (not at all) to 5 (greatly). Effector order randomized; body avatar image provided; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "lancaster_torso", "lancaster", "Torso.mean", "0–5", TRUE,
  "Strength of torso action associated with a word's meaning — the degree to which performing an action related to the concept involves the torso effector.",
  "'To what extent do you experience [WORD] by performing an action with the Torso?' Rated 0 (not at all) to 5 (greatly). Effector order randomized; body avatar image provided; 'Don't know this word' option available.",
  "Lynott, Connell, Brysbaert, Brand & Carney (2020). Behav Res Methods, 52, 1271–1291.",

  "socialness", "socialness", "Mean", "1–7", TRUE,
  "Socialness captures the degree to which a word's meaning is socially relevant, using an inclusive definition that encompasses social characteristics of persons or groups, social behaviours or interactions, social roles, spaces, institutions, values, and ideologies. It is conceptually distinct from concreteness and valence, and predicts unique variance in lexical decision performance.",
  "Participants rated the degree to which each word's meaning has social relevance by describing or referring to 'a social characteristic of a person or group of people, a social behaviour or interaction, a social role, a social space, a social institution or system, a social value or ideology, or any other socially relevant concept.' Rated on a 7-point Likert scale; 'I don't know the meaning of this word' option available.",
  "Diveica, Pexman & Binney (2023). Behav Res Methods, 55, 461–473.",

  "valence", "warriner_vad", "V.Mean.Sum", "1–9 (SAM)", TRUE,
  "Valence reflects the pleasantness or unpleasantness of the emotional response evoked by a word — the degree to which the word makes one feel happy versus unhappy. It is the most reliable of the three VAD dimensions and is right-skewed in English, reflecting a positivity bias in the lexicon.",
  "'You are invited to take part in the study that is investigating emotion, and concerns how people respond to different types of words. You will use a scale to rate how you felt while reading each word.' Scale: 1 (unhappy, annoyed, unsatisfied, melancholic, despaired, or bored) to 9 (happy, pleased, satisfied, contented, or hopeful). 'Please work at a rapid pace and don't spend too much time thinking about each word. Rather, make your ratings based on your first and immediate reaction.'",
  "Warriner, Kuperman & Brysbaert (2013). Behav Res Methods, 45, 1191–1207.",

  "arousal", "warriner_vad", "A.Mean.Sum", "1–9 (SAM)", TRUE,
  "Arousal reflects the intensity or activation of the emotional response evoked by a word — the degree to which the word makes one feel excited and stimulated versus calm and relaxed. Arousal is positively skewed, with most English words evoking relatively low arousal.",
  "Same task and scale as valence. Scale anchors: 1 (relaxed, calm, sluggish, dull, sleepy, or unaroused) to 9 (stimulated, excited, frenzied, jittery, wide-awake, or aroused). First and immediate reaction instructed.",
  "Warriner, Kuperman & Brysbaert (2013). Behav Res Methods, 45, 1191–1207.",

  "dominance", "warriner_vad", "D.Mean.Sum", "1–9 (SAM)", TRUE,
  "Dominance reflects the degree of control or power evoked by a word — whether the word denotes something that feels weak/submissive versus strong/dominant. Dominance is the most variable of the three VAD dimensions across raters.",
  "Same task and scale as valence. Scale anchors: 1 (controlled, influenced, cared-for, awed, submissive, or guided) to 9 (in control, influential, important, dominant, autonomous, or controlling). First and immediate reaction instructed.",
  "Warriner, Kuperman & Brysbaert (2013). Behav Res Methods, 45, 1191–1207.",

  "iconicity", "iconicity", "rating", "1–7", TRUE,
  "Iconicity reflects the degree to which a word's sound or orthographic form resembles or evokes its meaning (sound symbolism). Higher values indicate greater perceived resemblance between form and meaning. Iconicity is distinct from concreteness and imageability, and tends to be higher for words denoting sounds, motion, and basic physical experiences.",
  "Participants were asked to rate how much each word 'sounds like what it means' on a scale from 1 (the word does not sound like what it means at all) to 7 (the word very much sounds like what it means). A 'don't know' response option was available; such responses were excluded from the mean.",
  "Winter, Lupyan, Perry, Dingemanse & Perlman (2023). Behav Res Methods.",

  "glasgow_valence", "glasgow", "VAL", "1–9", TRUE,
  "Pleasantness of the feeling evoked by the word, from a single UK rater pool. Parallels the Warriner et al. valence norm but collected from University of Glasgow participants; retained separately to keep rater populations unmixed.",
  "Participants rated 'how positive or negative you would rate the emotion conveyed by the word' from 1 (very negative) to 9 (very positive).",
  "Scott, Keitel, Becirspahic, Yao & Sereno (2019). Behav Res Methods, 51, 1258–1270.",

  "glasgow_arousal", "glasgow", "AROU", "1–9", TRUE,
  "Intensity or activation of the feeling evoked by the word (UK rater pool). Parallels the Warriner et al. arousal norm.",
  "Participants rated 'how aroused (i.e., excited/calm) the word made them feel' from 1 (very calm) to 9 (very aroused).",
  "Scott, Keitel, Becirspahic, Yao & Sereno (2019). Behav Res Methods, 51, 1258–1270.",

  "glasgow_dominance", "glasgow", "DOM", "1–9", TRUE,
  "Sense of control or dominance implied by the word's meaning (UK rater pool). Parallels the Warriner et al. dominance norm.",
  "Participants rated 'how dominant/in control (versus controlled/submissive) the word made them feel' from 1 (very controlled) to 9 (very in control).",
  "Scott, Keitel, Becirspahic, Yao & Sereno (2019). Behav Res Methods, 51, 1258–1270.",

  "glasgow_concreteness", "glasgow", "CNC", "1–7", TRUE,
  "Degree to which the word's meaning is concrete (perceptible) versus abstract (UK rater pool). Parallels the Brysbaert et al. concreteness norm but on a 1–7 scale.",
  "Participants rated how concrete the word's meaning was from 1 (very abstract) to 7 (very concrete).",
  "Scott, Keitel, Becirspahic, Yao & Sereno (2019). Behav Res Methods, 51, 1258–1270.",

  "glasgow_imageability", "glasgow", "IMAG", "1–7", TRUE,
  "Ease with which the word evokes a sensory mental image (UK rater pool). Provides imageability with per-item SD and N on 5,553 words.",
  "Participants rated how easily the word evoked a mental image from 1 (very hard to imagine) to 7 (very easy to imagine).",
  "Scott, Keitel, Becirspahic, Yao & Sereno (2019). Behav Res Methods, 51, 1258–1270.",

  "glasgow_familiarity", "glasgow", "FAM", "1–7", TRUE,
  "Subjective familiarity — how often the rater encounters or uses the word (UK rater pool).",
  "Participants rated how familiar the word was from 1 (very unfamiliar) to 7 (very familiar).",
  "Scott, Keitel, Becirspahic, Yao & Sereno (2019). Behav Res Methods, 51, 1258–1270.",

  "glasgow_aoa", "glasgow", "AOA", "1–7", TRUE,
  "Subjective age of acquisition as a 1–7 band rating (UK rater pool). Distinct from the Kuperman `aoa` norm, which is scaled in years.",
  "Participants rated the age at which they learned the word on a 7-point band scale from 1 (learned earliest) to 7 (learned latest).",
  "Scott, Keitel, Becirspahic, Yao & Sereno (2019). Behav Res Methods, 51, 1258–1270.",

  "glasgow_size", "glasgow", "SIZE", "1–7", TRUE,
  "Semantic size — the perceived physical size of the word's referent (UK rater pool).",
  "Participants rated the size of the word's referent from 1 (very small) to 7 (very large).",
  "Scott, Keitel, Becirspahic, Yao & Sereno (2019). Behav Res Methods, 51, 1258–1270.",

  "glasgow_gender", "glasgow", "GEND", "1–7 (1=fem, 7=masc)", TRUE,
  "Perceived gender association of the word (UK rater pool). Runs 1 = feminine to 7 = masculine — the reverse of the `gender_femininity` norm (Roberts & Utych), where higher = more feminine.",
  "Participants rated the word's gender association from 1 (very feminine) to 7 (very masculine).",
  "Scott, Keitel, Becirspahic, Yao & Sereno (2019). Behav Res Methods, 51, 1258–1270.",

)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 4b: PER-STUDY PROVENANCE / PARTICIPANT TABLE
# ─────────────────────────────────────────────────────────────────────────────
# One row per source study included in the package (keyed to lexis_meta$dataset).
# Documents the rater pool behind each dataset so downstream users can filter
# norms by rater population (e.g., the US-MTurk homogeneity cut some analyses
# require) rather than relying on which datasets happen to be bundled.
# Participant figures transcribed from analysis/norming_participants.md.
lexis_datasets <- tribble(
  ~dataset,         ~citation,                                  ~year, ~n_words, ~n_participants_recruited, ~n_participants_final, ~platform,                                  ~age,                          ~country,        ~rater_population,        ~pay,
  "aoa",            "Kuperman, Stadthagen-Gonzalez & Brysbaert", 2012L, 30121L,  1960L,                     1729L,                 "Amazon Mechanical Turk",                   "15–82; ~47% aged 20–29",      "USA",           "US MTurk",               "$1.81/list",
  "boi",            "Pexman, Muraki, Sidhu, Siakaluk & Yap",     2019L, 9161L,   1258L,                     743L,                  "Amazon Mechanical Turk",                   "M = 37.4 (SD 11.1)",          "USA (assumed)", "MTurk (assumed US)",     "$2/list",
  "concreteness",   "Brysbaert, Warriner & Kuperman",            2014L, 37058L,  NA_integer_,               4237L,                 "Amazon Mechanical Turk",                   "36% aged 17–25; 57% female",  "USA",           "US MTurk",               "$0.75/list",
  "gender",         "Roberts & Utych",                           2019L, 700L,    NA_integer_,               175L,                  "Amazon Mechanical Turk",                   "not reported",                "USA (assumed)", "MTurk (assumed US)",     "$1/task",
  "glasgow",        "Scott, Keitel, Becirspahic, Yao & Sereno",  2019L, 5553L,   NA_integer_,               829L,                  "University of Glasgow online platform",    "M = 21.7 (SD 7.4); 16–73",    "UK",            "UK university",          "£6/h or credit",
  "humor",          "Engelthaler & Hills",                       2018L, 4997L,   950L,                      821L,                  "Amazon Mechanical Turk",                   "M = 35.4 (SD 11.7); 18–78",   "USA (assumed)", "MTurk (assumed US)",     "$1/task",
  "iconicity",      "Winter, Lupyan, Perry, Dingemanse & Perlman",2023L, 14776L,  NA_integer_,               1419L,                 "55% MTurk; 43% UW-Madison pool",           "M = 30 (SD 14); 18–88",       "USA",           "US (MTurk + university)","$0.60/set; credit",
  "lancaster",      "Lynott, Connell, Brysbaert, Brand & Carney",2020L, 39707L,  NA_integer_,               3500L,                 "Amazon Mechanical Turk",                   "M = 34.9 (SD 10.3); ~47% F",  "USA (assumed)", "MTurk (assumed US)",     "$2.25–2.75/list",
  "lexdec",         "Balota et al. (English Lexicon Project)",   2007L, 40481L,  NA_integer_,               816L,                  "6 US universities (LD n=816; naming n=444)","M = 22.9 (LD); 23.5 (naming)","USA",           "US university",          "$25 or credit",
  "socialness",     "Diveica, Pexman & Binney",                  2023L, 8388L,   605L,                      539L,                  "Prolific",                                 "M = 29.7 (SD 10.7); 18–76",   "UK",            "UK Prolific",            "£4/task",
  "warriner_vad",   "Warriner, Kuperman & Brysbaert",            2013L, 13915L,  NA_integer_,               1827L,                 "Amazon Mechanical Turk",                   "16–87; 45% aged 21–30; ~60% F","USA",          "US MTurk",               "$0.75/list"
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 5: ADD LEMMA COLUMN
# ─────────────────────────────────────────────────────────────────────────────
# Uses textstem::lemmatize_words(). Lemmatization is POS-unaware, so base-form
# words are usually unchanged and inflections may not always resolve.

word_lemmas <- tibble(word = unique(lexis_wide$word))
word_lemmas <- word_lemmas |>
  mutate(lemma = textstem::lemmatize_words(word))

lexis_long <- lexis_long |>
  left_join(word_lemmas, by = "word") |>
  relocate(lemma, .after = word)

lexis_wide <- lexis_wide |>
  left_join(word_lemmas, by = "word") |>
  relocate(lemma, .after = word)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 6: SAVE OUTPUTS
# ─────────────────────────────────────────────────────────────────────────────

build_dir <- file.path(base_dir, "data-raw/_build")
dir.create(build_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(lexis_long,     file.path(build_dir, "lexis_long.csv"))
write_csv(lexis_wide,     file.path(build_dir, "lexis_wide.csv"))
write_csv(lexis_meta,     file.path(build_dir, "lexis_meta.csv"))
write_csv(lexis_datasets, file.path(build_dir, "lexis_datasets.csv"))

message(
  "Done.\n",
  "  lexis_long     : ", nrow(lexis_long), " rows × ", ncol(lexis_long), " cols\n",
  "  lexis_wide     : ", nrow(lexis_wide), " words × ", ncol(lexis_wide), " cols\n",
  "  lexis_meta     : ", nrow(lexis_meta), " dimensions documented\n",
  "  lexis_datasets : ", nrow(lexis_datasets), " source studies\n",
  "  Dimensions     : ", paste(sort(unique(lexis_long$dimension)), collapse = ", ")
)
