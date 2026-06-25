if (!exists("find_base_dir", mode = "function")) {
  find_base_dir <- function() {
    env_dir <- Sys.getenv("LEXIS_BASE_DIR", unset = "")
    if (nzchar(env_dir)) return(normalizePath(env_dir, mustWork = TRUE))

    file_arg <- grep("^--file=", commandArgs(FALSE), value = TRUE)
    this_file <- if (length(file_arg)) sub("^--file=", "", file_arg[[1]]) else ""
    candidates <- unique(c(getwd(), dirname(normalizePath(this_file, mustWork = FALSE))))
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
}

base_dir <- find_base_dir()

library(dplyr)
library(jsonlite)

annotations_dir <- file.path(base_dir, "data-raw/_annotations")
annotation_paths <- list.files(annotations_dir, pattern = "\\.json$", full.names = TRUE)

if (length(annotation_paths) == 0) {
  stop("No annotation JSON files found in data-raw/_annotations.", call. = FALSE)
}

# norming_instructions documents the bundled rating-norm constructs only — it
# parallels the datasets in lexis_long. Annotation files for sources that are
# not rating norms (lexdec, prevalence) or were excluded for lacking per-item
# reliability (imageability, sensory-experience, verbs-in-space) are kept on
# disk for reference but skipped here. Keep this list in sync with lexis_long.
included_norm_datasets <- c(
  "aoa", "boi", "concreteness", "gender", "glasgow", "humor", "iconicity",
  "lancaster", "socialness", "valence-arousal-dominance"
)
annotation_paths <- annotation_paths[
  tools::file_path_sans_ext(basename(annotation_paths)) %in% included_norm_datasets
]

`%||%` <- function(x, y) if (is.null(x)) y else x

extract_construct_rows <- function(path) {
  payload <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  constructs <- payload$constructs
  if (is.null(constructs) || length(constructs) == 0) return(dplyr::tibble())

  dataset_id <- tools::file_path_sans_ext(basename(path))

  dplyr::bind_rows(lapply(constructs, function(x) {
    anchors_json <- if (is.null(x$scale$anchors)) {
      NA_character_
    } else {
      as.character(jsonlite::toJSON(x$scale$anchors, auto_unbox = TRUE))
    }

    dplyr::tibble(
      dataset               = dataset_id,
      construct_name        = x$construct_name        %||% NA_character_,
      n_items               = x$n_items               %||% NA_integer_,
      n_participants        = x$n_participants        %||% NA_integer_,
      scale_range           = x$scale$range           %||% NA_character_,
      scale_min             = as.numeric(x$scale_min  %||% NA_real_),
      scale_max             = as.numeric(x$scale_max  %||% NA_real_),
      scale_anchors         = anchors_json,
      instructions_verbatim = x$instructions_verbatim %||% NA_character_,
      instructions_llm      = x$instructions_llm      %||% NA_character_
    )
  }))
}

norming_instructions <- dplyr::bind_rows(lapply(annotation_paths, extract_construct_rows)) |>
  dplyr::arrange(dataset, construct_name)

out <- file.path(base_dir, "data", "norming_instructions.rda")
save(norming_instructions, file = out, compress = "xz")
message("Saved norming_instructions: ", nrow(norming_instructions), " rows -> ", out)
