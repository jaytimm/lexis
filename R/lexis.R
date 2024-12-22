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
    "concreteness",
    "age_of_acquisition",
    "morpholex",
    "free_association",
    "cmu_pronunciation",
    "wordset_dictionary",
    "glove50d"
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

# Individual dataset functions
#' Load Lexical Decision Dataset
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Lexical Decision dataset.
#' @export
lexical_decision <- function(...) {
  .load_data("lexical_decision", ...)
}

#' Load Concreteness Dataset
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Concreteness dataset.
#' @export
concreteness <- function(...) {
  .load_data("concreteness", ...)
}

#' Load Age of Acquisition Dataset
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Age of Acquisition dataset.
#' @export
age_of_acquisition <- function(...) {
  .load_data("age_of_acquisition", ...)
}

#' Load MorphoLex Dataset
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the MorphoLex dataset.
#' @export
morpholex <- function(...) {
  .load_data("morpholex", ...)
}

#' Load Free Association Dataset
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Free Association dataset.
#' @export
free_association <- function(...) {
  .load_data("free_association", ...)
}

#' Load CMU Pronunciation Dataset
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the CMU Pronunciation dataset.
#' @export
cmu_pronunciation <- function(...) {
  .load_data("cmu_pronunciation", ...)
}

#' Load Wordset Dictionary Dataset
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the Wordset Dictionary dataset.
#' @export
wordset_dictionary <- function(...) {
  .load_data("wordset_dictionary", ...)
}

#' Load GloVe Embeddings Dataset
#' @param ... Additional arguments passed to the internal `.load_data` function.
#' @return A data frame containing the GloVe Embeddings dataset.
#' @export
glove50d <- function(...) {
  .load_data("glove50d", ...)
}
