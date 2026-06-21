# Build note: populate Lancaster `n_ratings` in `lexis_long`

**Status:** to be applied in `data-raw/build_lexis.R` (§1g). Currently this logic
lives only in study-specific analysis code (see "Where applied so far"); it should
move into the package build so `lexis_long` ships with complete rater counts.

**Created:** 2026-06-02, from the paper-1 reliability analysis.

---

## Problem

`lexis_long` carries `n_ratings` (per-item rater count) for every norm dataset
**except Lancaster**, which is set to `NA`:

```r
# build_lexis.R, §1g (current)
mutate(
  dataset   = "lancaster",
  dimension = paste0("lancaster_", tolower(mod)),
  n_ratings = NA_integer_,        # <-- the gap
  scale_min = 0, scale_max = 5
)
```

The inline comment (line ~209) says *"N per item not in this file."* That is only
half true: the **per-modality** N is absent, but the source file does carry a
**per-modality-group** N that is the correct rater count for each dimension.

## Fix

The Lynott et al. (2020) design presents each word on either the **perceptual** list
(6 modalities) or the **action** list (5 modalities); a participant contributes
ratings only for words they report knowing. The source file
`datasets/lancaster/Lancaster_sensorimotor_norms_for_39707_words.csv` therefore
records, per word:

| Column | Meaning |
|---|---|
| `N_known.perceptual` | participants who knew the word and rated it on the 6 perceptual modalities |
| `N_known.action`     | participants who knew the word and rated it on the 5 action modalities |
| `List_N.*`           | participants the word was *presented* to (incl. "don't know") — **not** the rater count |
| `Percent_known.*`    | `N_known / List_N` |

Each modality's published `.mean` and `.SD` are computed over exactly the `N_known`
raters for its group, so `N_known` is the right `n_ratings` (the count contributing
to that mean/SD), consistent with how `n_ratings` is defined for the other datasets.

**Mapping:**

- Perceptual (`N_known.perceptual`): `auditory`, `gustatory`, `haptic`, `interoceptive`, `olfactory`, `visual`
- Action (`N_known.action`): `foot_leg`, `hand_arm`, `head`, `mouth`, `torso`

## Build change (§1g)

```r
lanc_perceptual <- c("Auditory","Gustatory","Haptic","Interoceptive","Olfactory","Visual")
# remaining lanc_modalities are the action set: Foot_leg, Hand_arm, Head, Mouth, Torso

lanc_long <- map_dfr(lanc_modalities, function(mod) {
  n_col <- if (mod %in% lanc_perceptual) "N_known.perceptual" else "N_known.action"
  lanc_raw |>
    transmute(
      word      = Word,
      dataset   = "lancaster",
      dimension = paste0("lancaster_", tolower(mod)),
      mean      = .data[[paste0(mod, ".mean")]],
      sd        = .data[[paste0(mod, ".SD")]],
      n_ratings = as.integer(.data[[n_col]]),
      scale_min = 0, scale_max = 5
    )
})
```

Also update the §1g comment (line ~209): the per-item N *is* recoverable, as
`N_known` for the perceptual/action modality group.

Then rebuild: run `data-raw/build_lexis.R` to regenerate `data/lexis_long.rda`
(and dependent `data-raw/_build/` artifacts).

## Verification

After rebuild:

```r
lexis_long |>
  dplyr::filter(dataset == "lancaster") |>
  dplyr::summarise(n_na = sum(is.na(n_ratings)), med = median(n_ratings, na.rm = TRUE))
# expect n_na = 0; median n_ratings ~18 (perceptual) / ~20 (action)
```

On the paper-1 holdout the recovered medians were 18 for the perceptual dimensions
and 20 for the action/effector dimensions.

## Where applied so far (study-specific — becomes redundant after this)

- `analysis/paper1/analysis-paper1-multimodel.Rmd`, the `reliability` chunk
  ("Norm Reliability Ceiling" section): it reads the raw Lancaster CSV and assigns
  `n_ratings` from `N_known.perceptual` / `N_known.action` per modality group, then
  computes the reliability of each published mean norm (`SD^2 / n_ratings` as the
  per-item measurement-error variance) for the Table 4 *Reliability* column.

Once the package build populates Lancaster `n_ratings`, that in-Rmd join can be
deleted and the analysis can read `n_ratings` straight from `lexis_long`.

## Downstream effects / notes

- Completes `n_ratings` coverage across all 18 analyzed norm dimensions in
  `lexis_long`; enables reliability-of-the-mean and SEM computations for Lancaster
  dimensions without special-casing.
- `n_ratings` is the same for all 6 perceptual dimensions of a given word (and for
  all 5 action dimensions), by design — it is a group-level count, not modality-
  specific. Document this in the data dictionary so users don't mistake it for a
  per-modality figure.
- Unrelated but adjacent: a few hundred `word × dimension` pairs are duplicated
  across source datasets (see the dedup handling in the SD analyses) — not affected
  by this change.
