# data-raw/build_package_data.R
# Converts .rds build outputs into .rda files in data/ for LazyData loading.
#
# Run ORDER:
#   1. data-raw/build_wordset.R   → xother/wordset-dictionary/wordset_{dict,index}.rds
#   2. data-raw/build_lexis.R     → lexis_{long,wide,meta}.rds
#   3. data-raw/build_glove.R     → data/glove50.rda
#   4. data-raw/build_package_data.R  ← this file

base_dir <- "/home/jtimm/Dropbox/working-papers/psycho-data"
data_dir <- file.path(base_dir, "data")
dir.create(data_dir, showWarnings = FALSE)

save_rda <- function(object_name, rds_path, compress = "xz") {
  if (!file.exists(rds_path)) {
    warning("Not found, skipping: ", rds_path)
    return(invisible(NULL))
  }
  obj <- readRDS(rds_path)
  assign(object_name, obj)
  out <- file.path(data_dir, paste0(object_name, ".rda"))
  save(list = object_name, file = out, compress = compress, envir = environment())
  message("Saved ", object_name, ": ", nrow(obj), " rows × ", ncol(obj), " cols → ", out)
  invisible(out)
}

# Core norm datasets
save_rda("lexis_long", file.path(base_dir, "lexis_long.rds"))
save_rda("lexis_wide", file.path(base_dir, "lexis_wide.rds"))
save_rda("lexis_meta", file.path(base_dir, "lexis_meta.rds"))

# Wordset dictionary
save_rda("wordset_dict",  file.path(base_dir, "xother/wordset-dictionary/wordset_dict.rds"))
save_rda("wordset_index", file.path(base_dir, "xother/wordset-dictionary/wordset_index.rds"))

# glove50 is already saved by build_glove.R — just report its size
glove_path <- file.path(data_dir, "glove50.rda")
if (file.exists(glove_path)) {
  message("glove50.rda already present: ",
          round(file.size(glove_path) / 1e6, 1), " MB")
} else {
  message("glove50.rda not found — run data-raw/build_glove.R first.")
}

message("\nAll data-raw → data/ conversions complete.")
message("Rebuild sequence:")
message("  Rscript data-raw/build_wordset.R")
message("  Rscript data-raw/build_lexis.R")
message("  Rscript data-raw/build_glove.R")
message("  Rscript data-raw/build_package_data.R")
