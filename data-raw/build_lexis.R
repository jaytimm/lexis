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
# Imageability (Bird, Franklin & Howard):
#   Uses 'new Imageability' column. '.' means no rating (mostly function words) → NA.
#   Scale is 100–700 (MRC Psycholinguistic Database convention). No SD or N per item.
#   'Word type' (POS tag: F=function, N=noun, V=verb, A=adjective) kept in wide format.
#
# Sensory Experience Rating (Juhasz & Yap):
#   Only mean SER is available in the .xls file; no SD or N per item.
#
# Verbs-in-Space (see brm.41.2.565.pdf):
#   299 verbs only. 'list' (experiment counterbalancing) and blank columns dropped.
#   Composite directional ratings (upwrd, dwnwrd, vert, left, right, horiz, toward, away)
#   go into the long format. Sub-measure components (atpdwnv, ttpupv, tbdwnv, abupv,
#   allacrh, tlracrh, trlacrh, arracrh) are FLAGGED FOR REVIEW — their precise
#   definitions require consulting the original paper. Retained in wide format only.
#   No fixed scale bounds documented; scale_min/scale_max set to NA.
#
# Gender (Roberts & Utych, 2019):
#   Scale 1–7 where higher = more feminine (1 = masculine, 7 = feminine).
#   'valence', 'arousal', 'dom' columns present in the XLSX but entirely NA — dropped.
#   'mean-a' is the combined-sex overall mean, used as primary dimension.
#
# Prevalence (Brysbaert et al., 2019):
#   Two dimensions extracted: Pknown (proportion 0–1) and Prevalence (logit-transformed
#   z-score, unbounded). FreqZipfUS retained in wide format as a covariate.
#
# Lexical Decision / English Lexicon Project (Balota et al., 2007):
#   I_Mean_RT = lexical decision RT; I_NMG_Mean_RT = naming RT. Units: milliseconds.
#   No bounded scale → scale_min/scale_max = NA.
#   Pron, NMG fields, z-scores, and accuracy columns retained in wide format.
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

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 1: READ & HARMONIZE EACH DATASET
# ─────────────────────────────────────────────────────────────────────────────

## 1a. Age of Acquisition — Kuperman et al. (2012) ────────────────────────────
# Columns: Word, OccurTotal (n lists), OccurNum (n raters), Rating.Mean, Rating.SD, Frequency
# Scale: years (~1–25). Frequency = SUBTLEX-US log frequency (retained in wide).
aoa_raw <- read_csv(
  file.path(base_dir, "aoa/Kuperman_et_al_2012.csv"),
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
  file.path(base_dir, "boi/13428_2018_1171_MOESM1_ESM.csv"),
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
  file.path(base_dir, "concreteness/Brysbaert_et_al_2014.csv"),
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
  file.path(base_dir, "gender/appendix_-_word_rating_file.xlsx")
)

gender <- gender_raw |>
  rename(word = Word, mean = `mean-a`, sd = `std.dev-a`, n_ratings = `n-all`) |>
  mutate(dataset = "gender", dimension = "gender_femininity",
         scale_min = 1, scale_max = 7) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1e. Humor — Engelthaler & Hills (2018) ─────────────────────────────────────
# Scale: 1 (not at all funny) to 5 (very funny).
humor_raw <- read_csv(
  file.path(base_dir, "humor/humor_dataset.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

humor <- humor_raw |>
  rename(n_ratings = n) |>
  mutate(dataset = "humor", dimension = "humor",
         scale_min = 1, scale_max = 5) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1f. Imageability — Bird, Franklin & Howard (2001) ──────────────────────────
# Scale: transformed 100–700 imageability score aligned to the MRC metric.
# Original ratings were made on a 1–7 scale. '.' = no rating → NA.
# No SD or N per item available. 'Word type' (POS) kept in wide format only.
img_raw <- read_csv(
  file.path(base_dir, "imageability/ratings.csv"),
  col_types = cols(),
  na = c(".", "", "NA"),
  show_col_types = FALSE
)

img <- img_raw |>
  rename(word = WORD, mean = `new Imageability`) |>
  mutate(
    dataset   = "imageability", dimension = "imageability",
    sd        = NA_real_, n_ratings = NA_integer_,
    scale_min = 100, scale_max = 700
  ) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1g. Lancaster Sensorimotor Norms — Lynott et al. (2020) ────────────────────
# 11 perceptual modalities + 11 action modalities, each with .mean and .SD.
# Scale: 0 (not at all) to 5 (greatly). N per item not in this file.
# Composite summaries (Max_strength, Minkowski3, Exclusivity, Dominant) kept in wide only.
lanc_raw <- read_csv(
  file.path(base_dir, "lancaster/Lancaster_sensorimotor_norms_for_39707_words.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

lanc_modalities <- c(
  "Auditory", "Gustatory", "Haptic", "Interoceptive",
  "Olfactory", "Visual", "Foot_leg", "Hand_arm", "Head", "Mouth", "Torso"
)

lanc_long <- map_dfr(lanc_modalities, function(mod) {
  lanc_raw |>
    select(Word, all_of(paste0(mod, ".mean")), all_of(paste0(mod, ".SD"))) |>
    rename(word = Word,
           mean = all_of(paste0(mod, ".mean")),
           sd   = all_of(paste0(mod, ".SD"))) |>
    mutate(
      dataset   = "lancaster",
      dimension = paste0("lancaster_", tolower(mod)),
      n_ratings = NA_integer_,
      scale_min = 0, scale_max = 5
    )
})

## 1h. English Lexicon Project — Balota et al. (2007) ─────────────────────────
# I_Mean_RT: lexical decision RT (ms); I_NMG_Mean_RT: naming RT (ms).
# No fixed scale bounds. Z-scores and accuracy retained in wide format.
lex_raw <- read_csv(
  file.path(base_dir, "lexdec/Balota_et_al_2007.csv"),
  col_types = cols(),
  na = c("NA", ""),
  show_col_types = FALSE
)

lex_ldt <- lex_raw |>
  select(Word, I_Mean_RT, I_SD, Obs) |>
  rename(word = Word, mean = I_Mean_RT, sd = I_SD, n_ratings = Obs) |>
  mutate(dataset = "lexdec", dimension = "lexdec_rt",
         scale_min = NA_real_, scale_max = NA_real_)

lex_nam <- lex_raw |>
  select(Word, I_NMG_Mean_RT, I_NMG_SD, I_NMG_Obs) |>
  rename(word = Word, mean = I_NMG_Mean_RT, sd = I_NMG_SD, n_ratings = I_NMG_Obs) |>
  mutate(dataset = "lexdec", dimension = "lexdec_naming_rt",
         scale_min = NA_real_, scale_max = NA_real_)

## 1i. Word Prevalence — Brysbaert et al. (2019) ───────────────────────────────
# Pknown: proportion of raters who know the word (0–1).
# Prevalence: logit-transformed z-score (unbounded continuous).
# FreqZipfUS = SUBTLEX-US Zipf frequency; retained in wide format as covariate.
prev_raw <- read_excel(
  file.path(base_dir, "prevalence/13428_2018_1077_MOESM2_ESM.xlsx")
)

prev_pknown <- prev_raw |>
  select(Word, Pknown, Nobs) |>
  rename(word = Word, mean = Pknown, n_ratings = Nobs) |>
  mutate(dataset = "prevalence", dimension = "prevalence_pknown",
         sd = NA_real_, scale_min = 0, scale_max = 1)

prev_score <- prev_raw |>
  select(Word, Prevalence, Nobs) |>
  rename(word = Word, mean = Prevalence, n_ratings = Nobs) |>
  mutate(dataset = "prevalence", dimension = "prevalence_score",
         sd = NA_real_, scale_min = NA_real_, scale_max = NA_real_)

## 1j. Sensory Experience Rating — Juhasz & Yap (2013) ────────────────────────
# Scale: 1 (no sensory experience) to 7 (strong sensory experience).
# Only mean available; no SD or N per item in this file.
ser_raw <- read_xls(
  file.path(base_dir, "sensory-experience/ser.xls")
)

ser <- ser_raw |>
  rename(word = Word, mean = `Average SER`) |>
  mutate(
    dataset   = "sensory_experience", dimension = "ser",
    sd        = NA_real_, n_ratings = NA_integer_,
    scale_min = 1, scale_max = 7
  ) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1k. Socialness — Diveica et al. (2023) ─────────────────────────────────────
# Scale: 1 (not at all social) to 7 (highly social).
soc_raw <- read_csv(
  file.path(base_dir, "socialness/Ratings_sum.stat.csv"),
  col_types = cols(),
  show_col_types = FALSE
)

soc <- soc_raw |>
  rename(word = Word, mean = Mean, sd = SD, n_ratings = N) |>
  mutate(dataset = "socialness", dimension = "socialness",
         scale_min = 1, scale_max = 7) |>
  select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max)

## 1l. Valence, Arousal, Dominance — Warriner et al. (2013) ───────────────────
# Scale: 1–9 (Self-Assessment Manikin). Full-sample summary (*.Sum) only.
# First column is an unnamed integer row index from original CSV — dropped automatically
# because it does not match any select() target.
vad_raw <- read_csv(
  file.path(base_dir, "valence-arousal-dominance/BRM-emot-submit.csv"),
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

## 1m. Verbs in Space (see verbs-in-space/brm.41.2.565.pdf) ───────────────────
# 299 verbs. Composite directional ratings → long format.
# Sub-measure components (atpdwnv, ttpupv, tbdwnv, abupv, allacrh, tlracrh,
# trlacrh, arracrh) are FLAGGED FOR REVIEW: consult brm.41.2.565.pdf for exact
# definitions. Retained in wide format only.
# 'list' (counterbalancing list assignment) dropped. Scale bounds not documented.
vis_raw <- read_tsv(
  file.path(base_dir, "verbs-in-space/MainNorms.txt"),
  col_types = cols(),
  na = c("", "NA"),
  show_col_types = FALSE
)

vis_composite_dims <- c("upwrd", "dwnwrd", "vert", "left", "right", "horiz", "toward", "away")

vis_long <- map_dfr(vis_composite_dims, function(d) {
  vis_raw |>
    select(verb, all_of(d)) |>
    rename(word = verb, mean = all_of(d)) |>
    mutate(
      dataset   = "verbs_in_space",
      dimension = paste0("vis_", d),
      sd        = NA_real_, n_ratings = NA_integer_,
      scale_min = NA_real_, scale_max = NA_real_
    )
})

## 1n. Wordset definition counts (requires data-raw/build_wordset.R to have been run) ──
# n_defs = total definitions across all POS for a word.
# n_pos  = number of distinct parts of speech attested for a word.
wordset_index_path <- file.path(
  base_dir, "xother/wordset-dictionary/wordset_index.rds"
)
if (!file.exists(wordset_index_path)) {
  stop("wordset_index.rds not found — run data-raw/build_wordset.R first.")
}
wordset_index_raw <- readRDS(wordset_index_path)

wordset_ndefs <- wordset_index_raw |>
  group_by(word = tolower(trimws(word))) |>
  summarise(
    total_defs = sum(n_defs),
    total_pos  = n(),
    .groups = "drop"
  )

ws_ndefs <- wordset_ndefs |>
  transmute(word, dataset = "wordset", dimension = "n_defs",
            mean = as.numeric(total_defs), sd = NA_real_,
            n_ratings = NA_integer_, scale_min = NA_real_, scale_max = NA_real_)

ws_npos <- wordset_ndefs |>
  transmute(word, dataset = "wordset", dimension = "n_pos",
            mean = as.numeric(total_pos), sd = NA_real_,
            n_ratings = NA_integer_, scale_min = NA_real_, scale_max = NA_real_)

## 1o. CMU Pronouncing Dictionary ─────────────────────────────────────────────
# Format: WORD  PH1 PH2 ... (two spaces). Stress on vowels: 0=unstressed,
# 1=primary, 2=secondary. Alternate pronunciations marked WORD(2), WORD(3), etc.
# Primary pronunciation only is used for n_phones and n_syllables.
# n_alts = total pronunciations listed (including primary); kept in wide only.
# Words with non-alpha characters in the headword (e.g. "!EXCLAMATION-POINT")
# are dropped after lowercasing — they won't match anything in the norms.
cmu_raw <- readLines(
  file.path(base_dir, "xother/cmu-pronunciation/cmudict-0.7b"),
  encoding = "latin1"
)
cmu_raw <- cmu_raw[!startsWith(cmu_raw, ";;;")]

cmu_parsed <- tibble(raw = cmu_raw) |>
  mutate(
    headword = sub("  .*", "", raw),
    phones   = sub("^[^  ]+  ", "", raw)
  ) |>
  mutate(
    is_alt   = grepl("\\([0-9]+\\)$", headword),
    base     = tolower(sub("\\([0-9]+\\)$", "", headword))
  )

# n_alts per word (count of all entries including primary)
cmu_alts <- cmu_parsed |>
  group_by(word = base) |>
  summarise(cmu_n_alts = n(), .groups = "drop")

# Primary pronunciation only
cmu_primary <- cmu_parsed |>
  filter(!is_alt) |>
  transmute(
    word       = base,
    cmu_arpabet = phones,
    n_phones   = lengths(strsplit(trimws(phones), " +")),
    n_syllables = lengths(regmatches(phones, gregexpr("[012]", phones)))
  )

# Drop headwords that contain non-letter characters after lowercasing
# (punctuation entries like !exclamation-point won't join to anything)
cmu_primary <- cmu_primary |>
  filter(grepl("^[a-z]", word))

cmu_data <- cmu_primary |>
  left_join(cmu_alts, by = "word")

cmu_nphones <- cmu_data |>
  transmute(word, dataset = "cmu", dimension = "n_phones",
            mean = as.numeric(n_phones), sd = NA_real_,
            n_ratings = NA_integer_, scale_min = NA_real_, scale_max = NA_real_)

cmu_nsyllables <- cmu_data |>
  transmute(word, dataset = "cmu", dimension = "n_syllables",
            mean = as.numeric(n_syllables), sd = NA_real_,
            n_ratings = NA_integer_, scale_min = NA_real_, scale_max = NA_real_)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 2: LONG-FORMAT TIDY DATAFRAME
# ─────────────────────────────────────────────────────────────────────────────

lexis_long <- bind_rows(
  aoa,
  boi,
  conc,
  gender,
  humor,
  img,
  lanc_long |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  lex_ldt   |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  lex_nam   |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  prev_pknown |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  prev_score  |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  ser,
  soc,
  vad_valence   |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  vad_arousal   |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  vad_dominance |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  vis_long |> select(word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max),
  ws_ndefs,
  ws_npos,
  cmu_nphones,
  cmu_nsyllables
) |>
  mutate(
    word      = tolower(trimws(word)),
    mean      = as.numeric(mean),
    sd        = as.numeric(sd),
    n_ratings = as.integer(n_ratings)
  ) |>
  filter(!is.na(word), word != "")

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

# 3b. Supplementary columns not in long format

aoa_supp <- aoa_raw |>
  transmute(word = tolower(trimws(Word)),
            aoa_frequency   = Frequency,
            aoa_occur_total = OccurTotal)

conc_supp <- conc_raw |>
  transmute(word = tolower(trimws(Word)),
            conc_bigram    = Bigram,
            conc_pct_known = Percent_known,
            conc_subtlex   = SUBTLEX)

lex_supp <- lex_raw |>
  transmute(word = tolower(trimws(Word)),
            lex_pos          = POS,
            lex_ld_zscore    = I_Zscore,
            lex_ld_accuracy  = I_Mean_Accuracy,
            lex_nam_zscore   = I_NMG_Zscore,
            lex_nam_accuracy = I_NMG_Mean_Accuracy)

prev_supp <- prev_raw |>
  transmute(word = tolower(trimws(Word)),
            prev_freq_zipf = FreqZipfUS)

gender_supp <- gender_raw |>
  transmute(word       = tolower(trimws(Word)),
            gender_pos = POS)

lanc_supp <- lanc_raw |>
  transmute(
    word                     = tolower(trimws(Word)),
    lanc_max_perceptual      = Max_strength.perceptual,
    lanc_mink3_perceptual    = Minkowski3.perceptual,
    lanc_exclusivity_perc    = Exclusivity.perceptual,
    lanc_dominant_perc       = Dominant.perceptual,
    lanc_max_action          = Max_strength.action,
    lanc_mink3_action        = Minkowski3.action,
    lanc_exclusivity_action  = Exclusivity.action,
    lanc_dominant_action     = Dominant.action,
    lanc_pct_known_perc      = Percent_known.perceptual,
    lanc_pct_known_action    = Percent_known.action
  )

# Verbs-in-space sub-measures (FLAGGED FOR REVIEW — see header note)
vis_supp <- vis_raw |>
  transmute(
    word          = tolower(trimws(verb)),
    vis_atpdwnv   = atpdwnv, vis_ttpupv  = ttpupv,
    vis_tbdwnv    = tbdwnv,  vis_abupv   = abupv,
    vis_allacrh   = allacrh, vis_tlracrh = tlracrh,
    vis_trlacrh   = trlacrh, vis_arracrh = arracrh
  )

img_supp <- img_raw |>
  transmute(word = tolower(trimws(WORD)),
            img_word_type = `Word type`)

# Join all supplementary columns
lexis_wide <- lexis_wide |>
  left_join(aoa_supp,    by = "word") |>
  left_join(conc_supp,   by = "word") |>
  left_join(lex_supp,    by = "word") |>
  left_join(prev_supp,   by = "word") |>
  left_join(gender_supp, by = "word") |>
  left_join(lanc_supp,   by = "word") |>
  left_join(vis_supp,    by = "word") |>
  left_join(img_supp,    by = "word") |>
  left_join(
    wordset_ndefs |> rename(ws_n_defs = total_defs, ws_n_pos = total_pos),
    by = "word"
  ) |>
  left_join(
    cmu_data |> select(word, cmu_arpabet, cmu_n_alts),
    by = "word"
  )

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

  "imageability", "imageability", "new Imageability", "100–700 (MRC scale)", FALSE,
  "The ease with which a word gives rise to a sensory mental image. High-imageability words (e.g., apple, thunder) readily evoke a vivid perceptual representation; low-imageability words (e.g., justice, belief) are difficult to picture or experience sensorially. Ratings were made on a 1–7 scale and linearly transformed to the MRC Psycholinguistic Database 100–700 scale.",
  "Participants rated each word on a 7-point scale (1 = least imageable, 7 = most imageable) following Gilhooly & Logie (1980) procedures. Words were presented in infinitival form (verbs) or preceded by a definite article (nouns) to disambiguate grammatical category. Ratings were then rescaled to 100–700.",
  "Bird, Franklin & Howard (2001). Behav Res Methods Instrum Comput, 33, 73–79.",

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

  "lexdec_rt", "lexdec", "I_Mean_RT", "ms (unbounded)", TRUE,
  "Mean lexical decision reaction time (ms) from the English Lexicon Project megastudy. Participants judged whether a letter string was a real English word or a nonword. Faster RTs indicate higher lexical accessibility; slower RTs are associated with lower frequency, later acquisition, and lower familiarity.",
  "Participants viewed letter strings one at a time and pressed one of two keys to indicate whether each string was a real English word or a pronounceable nonword, as quickly and accurately as possible. Presented individually; no time limit per trial.",
  "Balota et al. (2007). Behav Res Methods, 39, 445–459.",

  "lexdec_naming_rt", "lexdec", "I_NMG_Mean_RT", "ms (unbounded)", TRUE,
  "Mean word naming (reading aloud) reaction time (ms) from the English Lexicon Project. Participants read each word aloud as quickly as possible; onset latency was recorded. Naming times are sensitive to phonological and articulatory factors in addition to lexical variables.",
  "Participants viewed each word on screen and read it aloud as quickly and accurately as possible. A voice key recorded onset latency. Presented individually in the same megastudy session as the lexical decision task.",
  "Balota et al. (2007). Behav Res Methods, 39, 445–459.",

  "prevalence_pknown", "prevalence", "Pknown", "0–1 (proportion)", FALSE,
  "The proportion of English speakers who know a given word, estimated from a large-scale online vocabulary test. Word prevalence is distinct from frequency: some low-frequency words are widely known (e.g., armadillo) while some high-frequency words are known by fewer people. Prevalence predicts lexical decision times over and above frequency, word length, and AoA.",
  "For each letter string (words and nonwords intermixed), participants indicated 'yes' or 'no' whether they knew the stimulus. Feedback was provided at the end: vocabulary score = % words correctly identified minus % nonwords falsely accepted. Test available online at vocabulary.ugent.be; participants were native speakers of English from the US and UK.",
  "Brysbaert, Mandera, McCormick & Keuleers (2019). Behav Res Methods, 51, 1583–1603.",

  "prevalence_score", "prevalence", "Prevalence", "unbounded (probit z-score)", FALSE,
  "A probit-transformed z-score of word prevalence, derived from the proportion known (Pknown). The transformation maps the proportion to a normal distribution, giving a continuous unbounded score where 0 corresponds to a word known by 50% of the population. Negative values indicate words known by fewer than half of participants.",
  "Derived from Pknown using the probit function: NORM.INV(0.005 + Pknown × 0.99; 0; 1). No additional participant task; computed from the vocabulary test responses.",
  "Brysbaert, Mandera, McCormick & Keuleers (2019). Behav Res Methods, 51, 1583–1603.",

  "ser", "sensory_experience", "Average SER", "1–7", FALSE,
  "Sensory experience rating (SER) reflects the degree to which a word evokes a sensory or perceptual experience in the mind of the reader. Unlike imageability, which emphasizes visual and mental images, SER captures activation across all sensory modalities including taste, touch, sound, and smell. SER predicts lexical decision times independently of imageability, AoA, and BOI.",
  "Participants rated 'the degree to which each word evoked a sensory experience' on a 1 to 7 scale (1 = no sensory experience, 7 = strong sensory experience), specifically 'the ability for a word to evoke an actual sensation (taste, touch, sight, sound, or smell) you experience by reading the word.' Responses were untimed; each questionnaire took under one hour.",
  "Juhasz & Yap (2013). Behav Res Methods, 45, 160–168.",

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

  "vis_upwrd", "verbs_in_space", "upwrd", "continuous (no fixed bounds)", FALSE,
  "Composite rating of the degree to which the action denoted by a verb is associated with upward spatial motion or directionality. One of eight directional composites derived from spatial schema ratings for 299 English verbs.",
  "Participants rated the degree to which each verb was associated with particular spatial directions and schemas. Composite directional scores (upward, downward, vertical, left, right, horizontal, toward, away) were derived from sub-measure ratings. See brm.41.2.565.pdf for exact instruction wording.",
  "See verbs-in-space/brm.41.2.565.pdf.",

  "vis_dwnwrd", "verbs_in_space", "dwnwrd", "continuous (no fixed bounds)", FALSE,
  "Composite rating of the degree to which the action denoted by a verb is associated with downward spatial motion or directionality.",
  "See vis_upwrd instructions. Composite derived from sub-measure spatial ratings.",
  "See verbs-in-space/brm.41.2.565.pdf.",

  "vis_vert", "verbs_in_space", "vert", "continuous (no fixed bounds)", FALSE,
  "Composite rating of overall vertical spatial association for a verb's action (combining upward and downward components).",
  "See vis_upwrd instructions. Composite derived from sub-measure spatial ratings.",
  "See verbs-in-space/brm.41.2.565.pdf.",

  "vis_left", "verbs_in_space", "left", "continuous (no fixed bounds)", FALSE,
  "Composite rating of the degree to which the action denoted by a verb is associated with leftward spatial motion or directionality.",
  "See vis_upwrd instructions. Composite derived from sub-measure spatial ratings.",
  "See verbs-in-space/brm.41.2.565.pdf.",

  "vis_right", "verbs_in_space", "right", "continuous (no fixed bounds)", FALSE,
  "Composite rating of the degree to which the action denoted by a verb is associated with rightward spatial motion or directionality.",
  "See vis_upwrd instructions. Composite derived from sub-measure spatial ratings.",
  "See verbs-in-space/brm.41.2.565.pdf.",

  "vis_horiz", "verbs_in_space", "horiz", "continuous (no fixed bounds)", FALSE,
  "Composite rating of overall horizontal spatial association for a verb's action (combining left and right components).",
  "See vis_upwrd instructions. Composite derived from sub-measure spatial ratings.",
  "See verbs-in-space/brm.41.2.565.pdf.",

  "vis_toward", "verbs_in_space", "toward", "continuous (no fixed bounds)", FALSE,
  "Composite rating of the degree to which the action denoted by a verb is associated with motion toward the self or a reference point.",
  "See vis_upwrd instructions. Composite derived from sub-measure spatial ratings.",
  "See verbs-in-space/brm.41.2.565.pdf.",

  "vis_away", "verbs_in_space", "away", "continuous (no fixed bounds)", FALSE,
  "Composite rating of the degree to which the action denoted by a verb is associated with motion away from the self or a reference point.",
  "See vis_upwrd instructions. Composite derived from sub-measure spatial ratings.",
  "See verbs-in-space/brm.41.2.565.pdf.",

  "n_defs", "wordset", "n_defs (computed)", "count (unbounded)", FALSE,
  "Total number of definitions listed for a word across all parts of speech in the Wordset open English dictionary. A proxy for semantic richness and polysemy — words with more definitions tend to be more frequent, more concrete, and earlier acquired.",
  "Not a participant rating. Derived by summing n_defs across all POS entries per word in wordset_index.",
  "Wordset dictionary (github.com/wordset/wordset-dictionary).",

  "n_pos", "wordset", "n_pos (computed)", "count (unbounded)", FALSE,
  "Number of distinct parts of speech under which a word is listed in the Wordset dictionary. Reflects grammatical flexibility and is related to semantic ambiguity and frequency.",
  "Not a participant rating. Derived by counting distinct POS entries per word in wordset_index.",
  "Wordset dictionary (github.com/wordset/wordset-dictionary).",

  "n_phones", "cmu", "n_phones (computed)", "count (unbounded)", FALSE,
  "Total number of phonemes in the primary pronunciation of a word, derived from the CMU Pronouncing Dictionary ARPABET transcription. A standard measure of phonological length, distinct from orthographic length (number of letters).",
  "Not a participant rating. Derived by counting space-delimited phoneme tokens in the primary CMU ARPABET transcription.",
  "CMU Pronouncing Dictionary v0.7b (pronouncedict.com).",

  "n_syllables", "cmu", "n_syllables (computed)", "count (unbounded)", FALSE,
  "Number of syllables in the primary pronunciation of a word, derived from the count of stress-marked vowel nuclei in the CMU ARPABET transcription (digits 0, 1, 2 appended to vowel phonemes indicate stress level and mark syllable boundaries).",
  "Not a participant rating. Derived by counting stress-digit characters (0, 1, 2) in the primary CMU ARPABET transcription, each of which marks one syllable nucleus.",
  "CMU Pronouncing Dictionary v0.7b (pronouncedict.com)."
)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 5: ADD LEMMA COLUMN
# ─────────────────────────────────────────────────────────────────────────────
# Uses textstem::lemmatize_words() (Morphy/lexicon hash tables).
# Lemmatization is POS-unaware — defaults to noun rules — so verb inflections
# (e.g. "running" → "run") may not always resolve correctly, but base-form
# words (the majority of normed words) are returned unchanged.
# Computed once on the unique word set and joined to both frames.

library(textstem)

word_lemmas <- tibble(word = unique(lexis_wide$word)) |>
  mutate(lemma = lemmatize_words(word))

lexis_long <- lexis_long |>
  left_join(word_lemmas, by = "word") |>
  relocate(lemma, .after = word)

lexis_wide <- lexis_wide |>
  left_join(word_lemmas, by = "word") |>
  relocate(lemma, .after = word)

# ─────────────────────────────────────────────────────────────────────────────
# SECTION 6: SAVE OUTPUTS
# ─────────────────────────────────────────────────────────────────────────────

saveRDS(lexis_long, file.path(base_dir, "lexis_long.rds"))
saveRDS(lexis_wide, file.path(base_dir, "lexis_wide.rds"))
saveRDS(lexis_meta, file.path(base_dir, "lexis_meta.rds"))

write_csv(lexis_long, file.path(base_dir, "lexis_long.csv"))
write_csv(lexis_wide, file.path(base_dir, "lexis_wide.csv"))
write_csv(lexis_meta, file.path(base_dir, "lexis_meta.csv"))

message(
  "Done.\n",
  "  lexis_long : ", nrow(lexis_long), " rows × ", ncol(lexis_long), " cols\n",
  "  lexis_wide : ", nrow(lexis_wide), " words × ", ncol(lexis_wide), " cols\n",
  "  lexis_meta : ", nrow(lexis_meta), " dimensions documented\n",
  "  Dimensions : ", paste(sort(unique(lexis_long$dimension)), collapse = ", ")
)
