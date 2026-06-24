#' Psycholinguistic norms in long (tidy) format
#'
#' One row per word × dimension combination. Contains mean ratings, SDs, and
#' N per item wherever the source dataset provides them.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{word}{character. Lowercased, whitespace-trimmed word form.}
#'   \item{lemma}{character. Lemmatized form via textstem (noun-first Morphy rules).}
#'   \item{dataset}{character. Source dataset identifier (e.g., "aoa", "concreteness").}
#'   \item{dimension}{character. Specific norm dimension (e.g., "aoa", "valence").}
#'   \item{mean}{numeric. Mean rating or score.}
#'   \item{sd}{numeric. Standard deviation. NA if not provided by source.}
#'   \item{n_ratings}{integer. Number of raters/observations. NA if not provided.}
#'   \item{scale_min}{numeric. Minimum of the rating scale. NA if unbounded.}
#'   \item{scale_max}{numeric. Maximum of the rating scale. NA if unbounded.}
#' }
#' @source See \code{lexis_meta} for per-dimension citations.
"lexis_long"

#' Psycholinguistic norms in wide format
#'
#' One row per word, one column per dataset-dimension combination. Supplementary
#' columns (POS tags, frequency covariates, ARPABET transcriptions, etc.) are
#' appended after the core norm columns.
#'
#' @format A tibble with one row per word. Core norm columns follow the naming
#'   convention \code{<dimension>} (e.g., \code{aoa}, \code{valence},
#'   \code{lancaster_auditory}). Supplementary columns use \code{<dataset>_*}
#'   prefixes.
#' @source See \code{lexis_meta} for per-dimension citations.
"lexis_wide"

#' Metadata for all lexis norm dimensions
#'
#' Documents every dimension in \code{lexis_long} and \code{lexis_wide}: source
#' dataset, original column name, rating scale, SD availability, construct
#' description, participant instructions, and citation.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{dimension}{character. Dimension name matching \code{lexis_long$dimension}.}
#'   \item{dataset}{character. Source dataset identifier.}
#'   \item{original_col}{character. Column name in the source file.}
#'   \item{scale}{character. Scale description (e.g., "1-9 (SAM)").}
#'   \item{sd_available}{logical. Whether SD per item is available.}
#'   \item{construct}{character. Description of the psychological construct measured.}
#'   \item{instructions}{character. Participant instructions (quoted from source where available).}
#'   \item{citation}{character. Full citation for the source dataset.}
#' }
"lexis_meta"

#' GloVe word embeddings (2014, 300 dimensions) for the lexis vocabulary
#'
#' A numeric matrix with one row per word and 300 columns (GloVe dimensions).
#' Subset to words present in \code{lexis_wide}, the pretrained Pennington et
#' al. (2014) GloVe 6B vocabulary, and \code{glove2024}. Row names are the word
#' forms; row order matches \code{glove2024}.
#'
#' @format A named numeric matrix: words × 300 GloVe dimensions.
#' @source Pennington, Socher & Manning (2014). GloVe: Global vectors for word
#'   representation. EMNLP 2014. \url{https://nlp.stanford.edu/projects/glove/}
"glove2014"

#' GloVe word embeddings (2024, 300 dimensions) for the lexis vocabulary
#'
#' A numeric matrix with one row per word and 300 columns (GloVe dimensions).
#' Subset to words present in \code{lexis_wide}, the pretrained Carlson et al.
#' (2025) GloVe vocabulary, and \code{glove2014}. Row names are the word forms;
#' row order matches \code{glove2014}.
#'
#' @format A named numeric matrix: words × 300 GloVe dimensions.
#' @source Carlson, Bauer & Manning (2025). A new pair of GloVes.
#'   \url{https://nlp.stanford.edu/projects/glove/}
"glove2024"

#' Wordset dictionary: one row per word meaning (definition)
#'
#' Full definition-level data from the Wordset open English dictionary. Each row
#' is one meaning of one word.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{word}{character. Word form (lowercased).}
#'   \item{wordset_id}{character. Wordset internal word identifier.}
#'   \item{meaning_id}{character. Wordset internal meaning identifier.}
#'   \item{speech_part}{character. Part of speech (e.g., "noun", "verb").}
#'   \item{def}{character. Definition text.}
#'   \item{example}{character. Example sentence, if available.}
#'   \item{synonyms}{character. Semicolon-separated synonyms, if available.}
#'   \item{labels}{character. Semicolon-separated usage labels (e.g., "informal", "archaic").}
#' }
#' @source Wordset dictionary \url{https://github.com/wordset/wordset-dictionary}
"wordset_dict"

#' Construct-level instruction annotations from dataset JSON files
#'
#' Aggregated from `data-raw/_annotations/*.json` with one row per construct.
#' Includes source verbatim instruction text and a concise LLM-facing prompt.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{dataset}{character. Dataset identifier from annotation filename.}
#'   \item{construct_name}{character. Construct key (one row per construct).}
#'   \item{n_items}{integer. Number of items normed for the construct.}
#'   \item{n_participants}{integer. Number of participants, if reported.}
#'   \item{scale_range}{character. Scale range text (e.g., "1-7").}
#'   \item{scale_min}{numeric. Numeric lower bound of the scale, if bounded.}
#'   \item{scale_max}{numeric. Numeric upper bound of the scale, if bounded.}
#'   \item{scale_anchors}{character. JSON-encoded anchor labels for the scale.}
#'   \item{instructions_verbatim}{character. Verbatim participant instructions.}
#'   \item{instructions_llm}{character. Brief standardized LLM instruction text.}
#' }
"norming_instructions"

#' Wordset dictionary index: one row per word × part of speech
#'
#' Summary index of the Wordset dictionary: for each word and POS combination,
#' the number of definitions listed. These POS-specific counts are reference
#' metadata; most norming datasets are word-form ratings without sentence
#' context or explicit sense/POS disambiguation.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{word}{character. Word form (lowercased).}
#'   \item{pos}{character. Part of speech.}
#'   \item{n_defs}{integer. Number of definitions for this word-POS pair.}
#' }
#' @source Wordset dictionary \url{https://github.com/wordset/wordset-dictionary}
"wordset_index"
