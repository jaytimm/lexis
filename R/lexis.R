#' Download and Load Lexis Dataset
#'
#' This internal function downloads and loads an RDS file from the Lexis GitHub repository.
#' It supports temporary and persistent storage of the dataset.
#'
#' @param dataset_name A character string specifying the name of the dataset to load.
#' @param path A character string specifying the directory where the dataset should be stored.
#' If NULL, a temporary directory or a persistent directory (based on `use_persistent_storage`) is used.
#' @param use_persistent_storage A logical value indicating whether to store the dataset persistently.
#' Defaults to TRUE.
#' @param force_download A logical value indicating whether to re-download the dataset even if it exists locally.
#' Defaults to FALSE.
#' @return The requested dataset as an R object.
#' @noRd
.load_data <- function(dataset_name, path = NULL, use_persistent_storage = TRUE, force_download = FALSE) {
  # Base URL for datasets
  base_url <- "https://github.com/jaytimm/lexis/raw/refs/heads/main/data-raw/rds/"

  # Available datasets
  datasets <- c(
    "lexical_decision",
    "concreteness_ratings",
    "aoa_ratings",
    "morpholex",
    "free_association_norms",
    "cmu_pronunciation_dictionary",
    "wordset_dictionary",
    "glove_embeddings"
  )

  # Validate dataset name
  if (!dataset_name %in% datasets) {
    stop("Invalid dataset name. Available datasets are: ", paste(datasets, collapse = ", "))
  }

  # Determine storage path
  if (use_persistent_storage && is.null(path)) {
    path <- file.path(rappdirs::user_data_dir("lexis"), "data")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
  } else if (is.null(path)) {
    path <- tempdir()
  }

  # Define file paths
  file_url <- paste0(base_url, dataset_name, ".rds")
  local_file <- file.path(path, paste0(dataset_name, ".rds"))

  # Download the dataset if necessary
  if (!file.exists(local_file) || force_download) {
    message("Downloading dataset: ", dataset_name)
    utils::download.file(file_url, local_file, mode = "wb")
  }

  # Load and return the dataset
  readRDS(local_file)
}

#' Load Lexical Decision Dataset
#'
#' This function downloads and loads the Lexical Decision dataset.
#'
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Lexical Decision dataset.
#' @export
lexical_decision <- function(...) {
  .load_data("lexical_decision", ...)
}

#' Load Concreteness Ratings Dataset
#'
#' This function downloads and loads the Concreteness Ratings dataset.
#'
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Concreteness Ratings dataset.
#' @export
concreteness_ratings <- function(...) {
  .load_data("concreteness_ratings", ...)
}

#' Load Age of Acquisition Ratings Dataset
#'
#' This function downloads and loads the Age of Acquisition Ratings dataset.
#'
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Age of Acquisition Ratings dataset.
#' @export
aoa_ratings <- function(...) {
  .load_data("aoa_ratings", ...)
}

#' Load MorphoLex Dataset
#'
#' This function downloads and loads the MorphoLex dataset.
#'
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the MorphoLex dataset.
#' @export
morpholex <- function(...) {
  .load_data("morpholex", ...)
}

#' Load Free Association Norms Dataset
#'
#' This function downloads and loads the Free Association Norms dataset.
#'
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Free Association Norms dataset.
#' @export
free_association_norms <- function(...) {
  .load_data("free_association_norms", ...)
}

#' Load CMU Pronunciation Dictionary Dataset
#'
#' This function downloads and loads the CMU Pronunciation Dictionary dataset.
#'
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the CMU Pronunciation Dictionary dataset.
#' @export
cmu_pronunciation_dictionary <- function(...) {
  .load_data("cmu_pronunciation_dictionary", ...)
}

#' Load Wordset Dictionary Dataset
#'
#' This function downloads and loads the Wordset Dictionary dataset.
#'
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Wordset Dictionary dataset.
#' @export
wordset_dictionary <- function(...) {
  .load_data("wordset_dictionary", ...)
}

#' Load GloVe Embeddings Dataset
#'
#' This function downloads and loads the GloVe Embeddings dataset.
#'
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the GloVe Embeddings dataset.
#' @export
glove_embeddings <- function(...) {
  .load_data("glove_embeddings", ...)
}
