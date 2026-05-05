# NA

You are helping build an R package called lexis that compiles English
psycholinguistic norming datasets into a unified, well-documented
resource. The working directory contains a folder of folders. Each
subfolder has an intuitive name corresponding to a norming dataset
(e.g., warriner_vad, lancaster_sensorimotor). Each subfolder contains at
minimum one data file and one PDF article. Your tasks are as follows: 1.
INVENTORY: For each subfolder, identify and report: the dataset name,
the data file name and format, the number of rows, and all column names
exactly as they appear in the file. 2. CONCEPTUAL AUDIT: For each
dataset, read the PDF or infer from column names and dataset name: what
is the primary unit of observation (word, lemma, phrase), what
dimensions are measured, what scale is used for each dimension, whether
raw ratings or means are provided, whether SD and N per item are
included, and what the word identifier column is called. 3.
HARMONIZATION PLAN: Identify: which column corresponds to the word/lemma
identifier in each dataset, what renaming is needed to standardize the
word column to a common name (suggest word), what scale transformations
if any would be needed to put dimensions on comparable ranges, and which
datasets share overlapping dimensions that should be aligned. 4. R
AGGREGATION SCRIPT: Write a single R script called build_lexis.R that
reads all datasets, applies the harmonization plan, and produces: (a) a
long-format tidy dataframe with columns word, dataset, dimension, mean,
sd (NA if unavailable), n_ratings (NA if unavailable), scale_min,
scale_max; and (b) a wide-format dataframe with one row per word and
columns for each dataset-dimension combination using consistent naming
conventions. Include a metadata dataframe documenting each dimension:
dataset source, original column name, scale, and citation. 5.
DOCUMENTATION: Produce a markdown summary table listing every dataset,
every dimension, the scale, the N of items, whether SD is available, and
the source citation. This will become the package README. Do not drop
any columns silently — flag anything ambiguous for human review.
Prioritize transparency over automation. Where judgment calls are made,
document them explicitly in comments in the R script.
