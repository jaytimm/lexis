# data-raw/build_glove.R
# Reads the raw GloVe 50d text file, subsets to words present in lexis_wide,
# and saves a named numeric matrix as data/glove50.rda.
#
# Prerequisites:
#   - build_lexis.R must have been run (lexis_wide.rds must exist)
#   - xother/glove-embeddings/glove.6B.50d.txt must be present
#
# The raw file is ~164 MB (400k words × 50 dims). The subset saved to data/
# is typically ~30-50 MB compressed, small enough for GitHub.
#
# To load the full 400k vocabulary (e.g., for neighbor search beyond the norms
# vocabulary), use lex_load_glove() which reads the raw file directly.

library(dplyr)
library(readr)

base_dir  <- "/home/jtimm/Dropbox/working-papers/psycho-data"
glove_txt <- file.path(base_dir, "xother/glove-embeddings/glove.6B.50d.txt")
lexis_rds <- file.path(base_dir, "lexis_wide.rds")
out_path  <- file.path(base_dir, "data/glove50.rda")

if (!file.exists(glove_txt)) stop("GloVe text file not found: ", glove_txt)
if (!file.exists(lexis_rds)) stop("lexis_wide.rds not found — run build_lexis.R first.")

lexis_wide <- readRDS(lexis_rds)
norms_vocab <- unique(tolower(lexis_wide$word))

message("Reading GloVe 50d (400k words)...")
glove_raw <- data.table::fread(
  glove_txt,
  header    = FALSE,
  sep       = " ",
  quote     = "",
  data.table = FALSE,
  showProgress = TRUE
)

# Column 1 = word, columns 2–51 = embedding dimensions
words_raw <- glove_raw[[1]]
mat_raw   <- as.matrix(glove_raw[, -1])
rownames(mat_raw) <- words_raw

# Subset to norms vocabulary
keep     <- intersect(norms_vocab, rownames(mat_raw))
glove50  <- mat_raw[keep, , drop = FALSE]

message("  Raw vocab:        ", nrow(mat_raw))
message("  Norms vocab:      ", length(norms_vocab))
message("  Intersection:     ", nrow(glove50))
message("  Coverage:         ",
        round(100 * nrow(glove50) / length(norms_vocab), 1), "%")

save(glove50, file = out_path, compress = "xz")
message("Saved to: ", out_path)
message("  Object size: ",
        format(utils::object.size(glove50), units = "MB"))
