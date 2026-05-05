# Adding a New Dataset to lexis

## 1. Source data

- Drop the raw file(s) into `datasets/<dataset-name>/`
- Note the scale, column names, and any quirks in the `# JUDGMENT CALLS` block at the top of `data-raw/build_lexis.R`

## 2. `data-raw/build_lexis.R`

- Read and harmonise the new dataset into the standard schema:
  `word, dataset, dimension, mean, sd, n_ratings, scale_min, scale_max`
- All `word` values must be lowercased and whitespace-trimmed
- Bind the new dataset into `lexis_long` and join its columns into `lexis_wide`
- Add any wide-only supplementary columns (covariates, sub-measures, flags) after the core norm columns

## 3. `data-raw/build_all.R`

- No changes needed unless the new dataset introduces a new `.rds` prerequisite — if so, add it to `required_rds` and call `save_rda()` for it

## 4. Rebuild package data

```r
source("data-raw/build_all.R")   # regenerates lexis_long, lexis_wide, lexis_meta .rda
```

## 5. `R/data.R`

- Add a `@format` entry for any new package-level data objects introduced
- Update the description for `lexis_long`, `lexis_wide`, and `lexis_meta` if the dataset count or column set changed

## 6. `data-raw/_build/norming_instructions.jsonl`

- Add one or more records for the new dataset following `data-raw/norming_instructions_schema.json`
- Required fields: `dataset`, `instruction_id`, `instruction_type`, `status`, `source_pdf`, `source_pages`, `instruction_text_verbatim`, `notes`
- `status` should be `"extracted"` (verbatim), `"ambiguous"`, or `"unavailable"`

## 7. `data-raw/build_instructions.R`

```r
source("data-raw/build_instructions.R")   # regenerates data/norming_instructions.rda
```

## 8. `analysis/build-core-vocab.R`

- Add the new dimension name(s) to `target_dims`
- Rebuild the sample:

```r
source("analysis/build-core-vocab.R")   # regenerates analysis/output/norm_samples_long.csv
```

## 9. `analysis/config/tasks.yml`

- Add a task entry for the new dimension if you want LLM participants to rate it:

```yaml
- task_id: <dimension>_rating
  dataset: <dataset>
  dimension: <dimension>
  response_format: numeric
  response_range_min: <min>
  response_range_max: <max>
  output_column: response_numeric
  prompt_suffix: "Return only a number."
```

## 10. Rebuild embedding models (optional)

```r
source("analysis/build-embedding-models.R")   # fits ridge models for all dimensions in norm_samples_long.csv
```

## Checklist

- [ ] Raw file in `datasets/<dataset-name>/`
- [ ] `build_lexis.R` — read, harmonise, bind into long/wide
- [ ] `source("data-raw/build_all.R")` — package data rebuilt
- [ ] `R/data.R` — docs updated
- [ ] `norming_instructions.jsonl` — instructions added
- [ ] `source("data-raw/build_instructions.R")` — instructions rda rebuilt
- [ ] `build-core-vocab.R` — dimension added to `target_dims`, samples rebuilt
- [ ] `tasks.yml` — task entry added (if running LLM participants)
- [ ] `source("analysis/build-embedding-models.R")` — embedding models rebuilt
