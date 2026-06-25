#' Psycholinguistic norms in long (tidy) format
#'
#' One row per word × dimension combination. Contains \emph{rater-averaged human
#' rating norms only} — every dimension carries a per-item standard deviation and
#' rater count. Behavioural and corpus-derived measures (lexical-decision
#' latencies, word frequency, WordNet sense counts) are not norms and appear only
#' as covariate columns in \code{lexis_wide}. Per-study participant provenance is
#' documented in \code{lexis_datasets}.
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
#' One row per word. The vocabulary is defined by \code{lexis_long}: the norm
#' columns are \code{lexis_long} pivoted to one column per dimension (cell =
#' mean rating), so every word here carries at least one rating norm.
#' Non-norm covariates are left-joined after the norm columns.
#'
#' @format A tibble with one row per word. Norm columns follow the naming
#'   convention \code{<dimension>} (e.g., \code{aoa}, \code{valence},
#'   \code{lancaster_auditory}) and match \code{lexis_long$dimension}. The
#'   appended covariate columns are:
#' \describe{
#'   \item{lexdec_rt, lexdec_naming_rt}{numeric. Mean lexical-decision and
#'     speeded-naming reaction times (ms) from the English Lexicon Project
#'     (Balota et al., 2007). NA outside the ELP vocabulary.}
#'   \item{subtlex_us_zipf}{numeric. SUBTLEX-US Zipf word frequency from the
#'     Brysbaert et al. (2019) word-prevalence release.}
#'   \item{wordfreq_en_zipf}{numeric. Zipf-scale word frequency from the \pkg{wordfreq}
#'     package (Speer, 2023), aggregated across Wikipedia, subtitles, news,
#'     web, and social media sources. Scale ~1--7; NA for words not in the
#'     wordfreq vocabulary. Cite as: Speer, R. (2023). wordfreq v3.1.1.
#'     Zenodo. \doi{10.5281/zenodo.7199437}}
#'   \item{wn_n_synsets}{integer. Number of WordNet synsets (senses) for the
#'     word; a polysemy measure (Miller, 1995; Princeton WordNet 3.1).}
#'   \item{wn_n_noun, wn_n_verb, wn_n_adj, wn_n_adv}{integer. Synset counts
#'     within each part of speech; grammatical versatility is the number of
#'     these that are non-zero.}
#' }
#' @source See \code{lexis_meta} for per-dimension citations and
#'   \code{lexis_datasets} for per-study participant provenance.
"lexis_wide"

#' Metadata for all lexis norm dimensions
#'
#' Documents every rating-norm dimension in \code{lexis_long}: source dataset,
#' original column name, rating scale, SD availability, construct description,
#' participant instructions, and citation. One row per dimension.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{dimension}{character. Dimension name matching \code{lexis_long$dimension}.}
#'   \item{dataset}{character. Source dataset identifier (key to \code{lexis_datasets}).}
#'   \item{original_col}{character. Column name in the source file.}
#'   \item{scale}{character. Scale description (e.g., "1-9 (SAM)").}
#'   \item{sd_available}{logical. Whether SD per item is available.}
#'   \item{construct}{character. Description of the psychological construct measured.}
#'   \item{instructions}{character. Participant instructions (quoted from source where available).}
#'   \item{citation}{character. Full citation for the source dataset.}
#' }
"lexis_meta"

#' Per-study participant provenance for the lexis source datasets
#'
#' One row per source study included in the package, documenting the rater pool
#' behind each dataset. Lets users filter or weight norms by rater population
#' (e.g., restricting to a homogeneous US-MTurk subset) rather than relying on
#' which datasets happen to be bundled. Keys to \code{lexis_meta$dataset} and the
#' \code{<dataset>} provenance of \code{lexis_long}.
#'
#' @format A tibble with columns:
#' \describe{
#'   \item{dataset}{character. Source dataset identifier.}
#'   \item{citation}{character. Author short-form for the source study.}
#'   \item{year}{integer. Publication year.}
#'   \item{n_words}{integer. Number of words normed in the source study.}
#'   \item{n_participants_recruited}{integer. Participants recruited before
#'     exclusions, where reported (NA otherwise).}
#'   \item{n_participants_final}{integer. Participants in the analysed sample.}
#'   \item{platform}{character. Recruitment platform / venue.}
#'   \item{age}{character. Reported age summary.}
#'   \item{country}{character. Reported participant country.}
#'   \item{rater_population}{character. Concise rater-pool label for filtering
#'     (e.g., "US MTurk", "UK Prolific", "US university").}
#'   \item{pay}{character. Compensation, where reported.}
#' }
#' @source Transcribed from \code{data-raw/norming_participants.md}; see
#'   \code{lexis_meta$citation} for full references.
"lexis_datasets"

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

