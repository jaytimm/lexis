annotations_dir <- "data-raw/_annotations"
annotation_paths <- list.files(
  annotations_dir,
  pattern = "\\.json$",
  full.names = TRUE
)

if (length(annotation_paths) == 0) {
  stop("No annotation JSON files found in data-raw/_annotations.", call. = FALSE)
}

extract_construct_rows <- function(path) {
  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  constructs <- payload$constructs
  if (is.null(constructs) || length(constructs) == 0) {
    return(dplyr::tibble())
  }

  dataset_id <- tools::file_path_sans_ext(basename(path))

  dplyr::bind_rows(lapply(constructs, function(x) {
    anchors_json <- if (is.null(x$scale$anchors)) {
      NA_character_
    } else {
      as.character(jsonlite::toJSON(x$scale$anchors, auto_unbox = TRUE))
    }

    dplyr::tibble(
      dataset = dataset_id,
      construct_name = x$construct_name %||% NA_character_,
      n_items = x$n_items %||% NA_integer_,
      n_participants = x$n_participants %||% NA_integer_,
      scale_range = x$scale$range %||% NA_character_,
      scale_anchors = anchors_json,
      instructions_verbatim = x$instructions_verbatim %||% NA_character_,
      instructions_llm = x$instructions_llm %||% NA_character_
    )
  }))
}

`%||%` <- function(x, y) if (is.null(x)) y else x

norming_instructions <- dplyr::bind_rows(lapply(annotation_paths, extract_construct_rows)) |>
  dplyr::arrange(dataset, construct_name)

usethis::use_data(norming_instructions, overwrite = TRUE)
