# data-raw/build_all.R
# Full lexis package build pipeline.
#
# ── First-time setup (one-time only) ──────────────────────────────────────────
# WordNet columns (wn_*) require a Python step that must run after the first
# pass of build_lexis.R produces _build/lexis_wide.csv:
#
#   Rscript data-raw/build_lexis.R          # first pass (no wn_* yet)
#   python data-raw/build_wordnet.py        # needs: pip install nltk
#   Rscript data-raw/build_all.R            # full build with wn_* columns
#
# GloVe embeddings require large text files not in the repo:
#   datasets/xother/glove-embeddings/2014/glove.6B.300d.txt
#   datasets/xother/glove-embeddings/2024/wiki_giga_2024_300_...txt
# If absent, build_glove.R is skipped and data/glove*.rda are not updated.
#
# ── Normal rebuild ─────────────────────────────────────────────────────────────
# Once the above are in place, just run:
#   Rscript data-raw/build_all.R

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

build_dir <- file.path(base_dir, "data-raw/_build")
data_dir  <- file.path(base_dir, "data")
dir.create(data_dir, showWarnings = FALSE)

message("=== lexis build_all: ", base_dir, " ===\n")

# ── Step 1: Compile norming datasets ──────────────────────────────────────────
message("--- build_lexis.R ---")
source(file.path(base_dir, "data-raw/build_lexis.R"))

# ── Step 2: GloVe embeddings ───────────────────────────────────────────────────
message("\n--- build_glove.R ---")
source(file.path(base_dir, "data-raw/build_glove.R"))

# ── Step 3: Norming instructions ──────────────────────────────────────────────
message("\n--- build_instructions.R ---")
source(file.path(base_dir, "data-raw/build_instructions.R"))

# ── Step 4: Package _build CSVs → data/*.rda ──────────────────────────────────
message("\n--- Packaging .rda files ---")

required_csvs <- c(
  lexis_long     = file.path(build_dir, "lexis_long.csv"),
  lexis_wide     = file.path(build_dir, "lexis_wide.csv"),
  lexis_meta     = file.path(build_dir, "lexis_meta.csv"),
  lexis_datasets = file.path(build_dir, "lexis_datasets.csv")
)
missing <- required_csvs[!file.exists(required_csvs)]
if (length(missing)) {
  stop(
    "Missing _build CSVs:\n",
    paste0("  ", missing, collapse = "\n"),
    call. = FALSE
  )
}

save_rda <- function(object_name, csv_path, compress = "xz") {
  obj <- readr::read_csv(csv_path, col_types = readr::cols(), show_col_types = FALSE)
  assign(object_name, obj)
  out <- file.path(data_dir, paste0(object_name, ".rda"))
  save(list = object_name, file = out, compress = compress, envir = environment())
  message("Saved ", object_name, ": ", nrow(obj), " rows x ", ncol(obj), " cols")
  invisible(out)
}

save_rda("lexis_long",     required_csvs[["lexis_long"]])
save_rda("lexis_wide",     required_csvs[["lexis_wide"]])
save_rda("lexis_meta",     required_csvs[["lexis_meta"]])
save_rda("lexis_datasets", required_csvs[["lexis_datasets"]])

message("\n=== build_all finished ===")
