#' Get GloVe embedding vector(s) for one or more words
#'
#' Returns a matrix of 50-dimensional GloVe embeddings for the requested words.
#' Words not present in \code{glove50} are silently dropped (check the row
#' names of the result to see which words were found).
#'
#' @param words character vector. One or more words (case-insensitive).
#'
#' @return A numeric matrix with one row per found word and 50 columns. Row
#'   names are the matched word forms. Returns NULL with a warning if no words
#'   are found.
#'
#' @examples
#' lex_embed("dog")
#' lex_embed(c("king", "queen", "man", "woman"))
#'
#' @export
lex_embed <- function(words) {
  words  <- tolower(trimws(words))
  found  <- intersect(words, rownames(glove50))
  if (length(found) == 0) {
    warning("No words found in glove50.")
    return(NULL)
  }
  if (length(found) < length(words)) {
    missing <- setdiff(words, found)
    warning(length(missing), " word(s) not in glove50: ",
            paste(head(missing, 5), collapse = ", "),
            if (length(missing) > 5) "..." else "")
  }
  glove50[found, , drop = FALSE]
}

#' Find nearest embedding neighbors for a target word
#'
#' Computes cosine similarity between a target word's GloVe vector and all
#' other words in \code{glove50} (or a subset), returning the top-N most
#' similar words.
#'
#' @param word character. A single target word.
#' @param n integer. Number of nearest neighbors to return (default 10).
#'   The target word itself is excluded from results.
#' @param candidates character vector or NULL. If not NULL, restrict the search
#'   to this set of words. Useful for finding neighbors among normed words only
#'   (e.g., \code{candidates = lexis_wide$word}).
#'
#' @return A tibble with columns: word, similarity (cosine, higher = more
#'   similar). Sorted descending by similarity.
#'
#' @examples
#' lex_neighbors("happy")
#' lex_neighbors("run", n = 20, candidates = lexis_wide$word)
#'
#' @export
lex_neighbors <- function(word, n = 10, candidates = NULL) {
  word <- tolower(trimws(word))
  if (!word %in% rownames(glove50)) {
    stop("'", word, "' not found in glove50.")
  }

  search_mat <- if (!is.null(candidates)) {
    cands <- intersect(tolower(candidates), rownames(glove50))
    cands <- setdiff(cands, word)
    glove50[cands, , drop = FALSE]
  } else {
    glove50[setdiff(rownames(glove50), word), , drop = FALSE]
  }

  target  <- glove50[word, , drop = FALSE]
  sims    <- .cosine_sim(target, search_mat)
  sim_vals <- as.numeric(sims[1, ])
  sim_words <- colnames(sims)

  tibble::tibble(
    word       = sim_words,
    similarity = sim_vals
  ) |>
    dplyr::filter(!is.na(.data$word), nzchar(.data$word)) |>
    dplyr::arrange(dplyr::desc(.data$similarity)) |>
    utils::head(n)
}

# Internal: cosine similarity between row vectors of a and row vectors of b
# Returns an nrow(a) × nrow(b) matrix.
.cosine_sim <- function(a, b) {
  a_norm <- a / sqrt(rowSums(a^2))
  b_norm <- b / sqrt(rowSums(b^2))
  tcrossprod(a_norm, b_norm)
}
