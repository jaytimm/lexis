#' Look up psycholinguistic norms for one or more words
#'
#' Returns a tidy (long) tibble of norm values for the requested words.
#' By default returns all available dimensions; use \code{dims} to restrict.
#'
#' @param words character vector. One or more words to look up (case-insensitive).
#' @param dims character vector or NULL. Dimensions to return. If NULL, all
#'   dimensions are returned. See \code{lexis_meta$dimension} for valid names.
#' @param wide logical. If TRUE, pivot to wide format (one row per word).
#'   Default FALSE (long format).
#'
#' @return A tibble. In long format: word, lemma, dataset, dimension, mean, sd,
#'   n_ratings, scale_min, scale_max. In wide format: word, lemma, then one
#'   column per dimension.
#'
#' @examples
#' lex_lookup("dog")
#' lex_lookup(c("dog", "cat"), dims = c("aoa", "concreteness", "valence"))
#' lex_lookup("run", wide = TRUE)
#'
#' @export
lex_lookup <- function(words, dims = NULL, wide = FALSE) {
  words <- tolower(trimws(words))
  out <- dplyr::filter(lexis_long, .data$word %in% words)
  if (!is.null(dims)) {
    out <- dplyr::filter(out, .data$dimension %in% dims)
  }
  if (wide) {
    out <- tidyr::pivot_wider(
      dplyr::select(out, word, lemma, dimension, mean),
      names_from  = dimension,
      values_from = mean
    )
  }
  out
}

#' Search for words matching a pattern
#'
#' Returns words in \code{lexis_wide} whose word form matches a regular
#' expression. Useful for finding all words starting with a string, all words
#' ending in a suffix, etc.
#'
#' @param pattern character. A regular expression (passed to \code{grepl}).
#' @param ignore_case logical. If TRUE (default), case-insensitive matching.
#' @param cols character vector or NULL. Columns to return from \code{lexis_wide}.
#'   If NULL (default), returns word and lemma only.
#'
#' @return A tibble of matching words with the requested columns.
#'
#' @examples
#' lex_search("^un")       # words starting with "un"
#' lex_search("ness$")     # words ending in "-ness"
#' lex_search("^[aeiou]")  # words starting with a vowel
#'
#' @export
lex_search <- function(pattern, ignore_case = TRUE, cols = NULL) {
  matched <- dplyr::filter(
    lexis_wide,
    grepl(pattern, .data$word, ignore.case = ignore_case)
  )
  if (!is.null(cols)) {
    cols <- intersect(c("word", "lemma", cols), names(matched))
    matched <- dplyr::select(matched, dplyr::all_of(cols))
  } else {
    matched <- dplyr::select(matched, word, lemma)
  }
  matched
}

#' Filter lexis_wide by norm thresholds
#'
#' Subset \code{lexis_wide} to words that meet all specified threshold conditions.
#' Conditions are evaluated as minimum values (>=) by default; use named list
#' elements with \code{list(min = x, max = y)} to specify a range.
#'
#' @param ... Named arguments where each name is a dimension column in
#'   \code{lexis_wide} and each value is either a single numeric (treated as
#'   minimum) or a length-2 numeric vector \code{c(min, max)}.
#' @param na.rm logical. If TRUE (default), words with NA on any filtered
#'   dimension are excluded.
#'
#' @return A tibble: rows of \code{lexis_wide} satisfying all conditions,
#'   plus word and lemma columns.
#'
#' @examples
#' # high concreteness, high valence
#' lex_filter(concreteness = 4, valence = 7)
#'
#' # words with aoa between 3 and 8 and low arousal
#' lex_filter(aoa = c(3, 8), arousal = c(1, 3))
#'
#' # abstract (low concreteness) words with many definitions
#' lex_filter(concreteness = c(1, 2), n_defs = 5)
#'
#' @export
lex_filter <- function(..., na.rm = TRUE) {
  conds <- list(...)
  out   <- lexis_wide
  for (nm in names(conds)) {
    if (!nm %in% names(out)) {
      warning("Column '", nm, "' not found in lexis_wide — skipping.")
      next
    }
    rng <- conds[[nm]]
    if (na.rm) out <- dplyr::filter(out, !is.na(.data[[nm]]))
    if (length(rng) == 1) {
      out <- dplyr::filter(out, .data[[nm]] >= rng[1])
    } else {
      out <- dplyr::filter(out, .data[[nm]] >= rng[1], .data[[nm]] <= rng[2])
    }
  }
  out
}
