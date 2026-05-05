# data-raw/build_all.R
# Runs the full pipeline in order.

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

base_dir <- find_base_dir()
owd <- getwd()
on.exit(setwd(owd), add = TRUE)
setwd(base_dir)
message("=== lexis build_all: ", base_dir, " ===\n")

source(file.path(base_dir, "data-raw/build_wordset.R"))
source(file.path(base_dir, "data-raw/build_lexis.R"))
source(file.path(base_dir, "data-raw/build_glove.R"))

message("\n=== Packaging LazyData objects ===")
data_dir <- file.path(base_dir, "data")
build_dir <- file.path(base_dir, "data-raw/_build")
dir.create(data_dir, showWarnings = FALSE)

required_rds <- c(
  lexis_long    = file.path(build_dir, "lexis_long.rds"),
  lexis_wide    = file.path(build_dir, "lexis_wide.rds"),
  lexis_meta    = file.path(build_dir, "lexis_meta.rds"),
  wordset_dict  = file.path(base_dir, "datasets/xother/wordset-dictionary/wordset_dict.rds"),
  wordset_index = file.path(base_dir, "datasets/xother/wordset-dictionary/wordset_index.rds")
)
missing <- required_rds[!file.exists(required_rds)]
if (length(missing)) {
  stop(
    "Missing prerequisite .rds files under:\n  ", base_dir, "\n\n",
    "Absent:\n",
    paste0("  - ", names(missing), " -> ", missing, collapse = "\n"),
    call. = FALSE
  )
}

save_rda <- function(object_name, rds_path, compress = "xz") {
  obj <- readRDS(rds_path)
  assign(object_name, obj)
  out <- file.path(data_dir, paste0(object_name, ".rda"))
  save(list = object_name, file = out, compress = compress, envir = environment())
  dim_txt <- if (is.matrix(obj)) {
    paste0(nrow(obj), " x ", ncol(obj), " matrix")
  } else {
    paste0(nrow(obj), " rows x ", ncol(obj), " cols")
  }
  message("Saved ", object_name, ": ", dim_txt, " -> ", out)
  invisible(out)
}

save_rda("lexis_long",    required_rds[["lexis_long"]])
save_rda("lexis_wide",    required_rds[["lexis_wide"]])
save_rda("lexis_meta",    required_rds[["lexis_meta"]])
save_rda("wordset_dict",  required_rds[["wordset_dict"]])

glove_path <- file.path(data_dir, "glove50.rda")
if (file.exists(glove_path)) {
  message("glove50.rda present: ", round(file.size(glove_path) / 1e6, 1), " MB")
} else {
  warning(
    "data/glove50.rda not found; run data-raw/build_glove.R after data-raw/_build/lexis_wide.rds exists.",
    call. = FALSE
  )
}

message("\n=== build_all finished ===")
