#' Match stimulus words on psycholinguistic properties
#'
#' Given a set of target words and a pool of candidate words, finds the best
#' match(es) in the pool for each target on a specified set of norm dimensions,
#' using standardized Euclidean distance. Useful for constructing matched word
#' lists in experimental designs (e.g., matched on AoA and concreteness but
#' differing on valence).
#'
#' Words with missing values on any requested dimension are silently excluded
#' from the candidate pool. Targets with missing values receive NA matches.
#'
#' @param targets character vector. Words to find matches for.
#' @param pool character vector or NULL. Candidate words to match against. If
#'   NULL (default), the full \code{lexis_wide} vocabulary minus the targets
#'   is used.
#' @param dims character vector. Norm dimensions to match on. Must be column
#'   names in \code{lexis_wide} (e.g., \code{c("aoa", "concreteness")}).
#' @param n integer. Number of matches to return per target (default 1).
#' @param exclude character vector or NULL. Additional words to exclude from
#'   the candidate pool (e.g., practice items).
#'
#' @return A tibble with columns: target, match (word form of best match),
#'   distance (standardized Euclidean distance; lower = better match), and
#'   one column per requested dimension showing the matched word's value.
#'   If \code{n > 1}, multiple rows per target are returned.
#'
#' @examples
#' # Match targets on AoA and concreteness
#' lex_match(c("knife", "hammer"), dims = c("aoa", "concreteness"))
#'
#' # Match within a restricted pool, get top 3
#' my_pool <- lex_filter(concreteness = c(3, 5))$word
#' lex_match("storm", pool = my_pool, dims = c("aoa", "valence"), n = 3)
#'
#' @export
lex_match <- function(targets, pool = NULL, dims, n = 1, exclude = NULL) {
  targets <- tolower(trimws(targets))

  if (is.null(pool)) {
    pool <- lexis_wide$word
  }
  pool <- setdiff(tolower(pool), c(targets, exclude))

  needed_cols <- c("word", dims)
  wide_sub    <- dplyr::select(lexis_wide, dplyr::all_of(needed_cols))

  target_df <- dplyr::filter(wide_sub, .data$word %in% targets)
  pool_df   <- dplyr::filter(wide_sub, .data$word %in% pool)

  # Drop pool rows with any NA on matching dims
  pool_df <- pool_df[stats::complete.cases(pool_df[, dims, drop = FALSE]), ]

  if (nrow(pool_df) == 0) stop("No complete cases in pool for the requested dimensions.")

  # Standardize using pool SDs (so each dimension contributes equally)
  pool_mat <- as.matrix(pool_df[, dims, drop = FALSE])
  sds      <- apply(pool_mat, 2, stats::sd, na.rm = TRUE)
  sds[sds == 0] <- 1  # constant columns: no contribution

  pool_scaled   <- sweep(pool_mat,   2, sds, "/")
  target_mat    <- as.matrix(target_df[, dims, drop = FALSE])
  target_scaled <- sweep(target_mat, 2, sds, "/")

  results <- purrr::map_dfr(seq_len(nrow(target_df)), function(i) {
    tgt_word <- target_df$word[i]
    tgt_vec  <- target_scaled[i, ]
    if (anyNA(tgt_vec)) {
      return(tibble::tibble(target = tgt_word, match = NA_character_,
                            distance = NA_real_))
    }
    dists <- sqrt(rowSums(sweep(pool_scaled, 2, tgt_vec)^2))
    ord   <- order(dists)[seq_len(min(n, length(dists)))]
    tibble::tibble(
      target   = tgt_word,
      match    = pool_df$word[ord],
      distance = dists[ord]
    ) |>
      dplyr::bind_cols(
        tibble::as_tibble(pool_df[ord, dims, drop = FALSE])
      )
  })

  results
}
