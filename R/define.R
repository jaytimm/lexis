#' Retrieve dictionary definitions for one or more words
#'
#' Looks up entries in \code{wordset_dict} and returns definitions, parts of
#' speech, example sentences, and synonyms. Multiple senses are returned as
#' separate rows.
#'
#' @param words character vector. One or more words to define (case-insensitive).
#' @param pos character vector or NULL. Filter to specific parts of speech
#'   (e.g., \code{"noun"}, \code{"verb"}). If NULL (default), all POS returned.
#' @param n integer or NULL. If not NULL, return at most this many definitions
#'   per word (the first \code{n} listed).
#' @param include_examples logical. If FALSE, the \code{example} column is
#'   dropped. Default TRUE.
#' @param include_synonyms logical. If FALSE, the \code{synonyms} column is
#'   dropped. Default TRUE.
#'
#' @return A tibble with columns: word, speech_part, def, example (optional),
#'   synonyms (optional), labels.
#'
#' @examples
#' lex_define("bank")
#' lex_define("run", pos = "verb", n = 3)
#' lex_define(c("love", "hate"), include_examples = FALSE)
#'
#' @export
lex_define <- function(words, pos = NULL, n = NULL,
                       include_examples = TRUE, include_synonyms = TRUE) {
  words <- tolower(trimws(words))
  out   <- dplyr::filter(wordset_dict, .data$word %in% words)

  if (!is.null(pos)) {
    out <- dplyr::filter(out, .data$speech_part %in% pos)
  }

  if (!is.null(n)) {
    out <- out |>
      dplyr::group_by(.data$word, .data$speech_part) |>
      dplyr::slice_head(n = n) |>
      dplyr::ungroup()
  }

  keep_cols <- c("word", "speech_part", "def")
  if (include_examples)  keep_cols <- c(keep_cols, "example")
  if (include_synonyms)  keep_cols <- c(keep_cols, "synonyms")
  keep_cols <- c(keep_cols, "labels")

  dplyr::select(out, dplyr::all_of(keep_cols))
}
