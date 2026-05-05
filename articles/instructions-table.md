# Instruction Table

This vignette provides an interactive table of participant-facing
instruction text from the LLM extraction artifact:
`data-raw/_build/norming_instructions.jsonl`.

``` r

library(dplyr)
#> 
#> Attaching package: 'dplyr'
#> The following objects are masked from 'package:stats':
#> 
#>     filter, lag
#> The following objects are masked from 'package:base':
#> 
#>     intersect, setdiff, setequal, union
library(jsonlite)
library(DT)
```

## Build instruction table

``` r

resolve_instructions_path <- function() {
  candidates <- c(
    "data-raw/_build/norming_instructions.jsonl",
    "../data-raw/_build/norming_instructions.jsonl",
    "../../data-raw/_build/norming_instructions.jsonl"
  )
  found <- candidates[file.exists(candidates)]
  if (!length(found)) {
    stop("Could not find data-raw/_build/norming_instructions.jsonl")
  }
  found[[1]]
}

jsonl_path <- resolve_instructions_path()
json_lines <- readLines(jsonl_path, warn = FALSE, encoding = "UTF-8")
json_lines <- json_lines[nzchar(trimws(json_lines))]

instruction_tbl <- bind_rows(lapply(json_lines, jsonlite::fromJSON)) |>
  transmute(
    dataset,
    instruction_id,
    instruction_type,
    status,
    source_pdf,
    source_pages,
    instruction_text_verbatim,
    notes
  ) |>
  arrange(dataset, instruction_id)

instruction_tbl
#> # A tibble: 15 × 8
#>    dataset        instruction_id instruction_type status source_pdf source_pages
#>    <chr>          <chr>          <chr>            <chr>  <chr>      <chr>       
#>  1 aoa            aoa_2012_prom… word_rating_ins… extra… /home/jti… 3           
#>  2 aoa            aoa_2025_prin… word_rating_ins… extra… /home/jti… 4-5         
#>  3 boi            boi_appendix_… semantic_rating… extra… /home/jti… 12          
#>  4 concreteness   concreteness_… semantic_rating… extra… /home/jti… 3           
#>  5 gender         gender_word_r… gender_rating_i… extra… /home/jti… 4           
#>  6 humor          humor_instruc… affective_ratin… extra… /home/jti… 3           
#>  7 imageability   imageability_… imageability_ao… extra… /home/jti… 3           
#>  8 lancaster      lancaster_sen… sensorimotor_ra… extra… /home/jti… 5           
#>  9 lexdec         elp_lexdec_na… lexical_decisio… extra… /home/jti… 2-4         
#> 10 prevalence     prevalence_ye… word_knowledge_… extra… /home/jti… 2           
#> 11 sensory-exper… ser_instructi… sensory_rating_… extra… /home/jti… 7           
#> 12 socialness     socialness_in… social_semantic… extra… /home/jti… 3           
#> 13 valence-arous… vad_instructi… vad_rating_inst… extra… /home/jti… 3           
#> 14 verbs-in-space verbs_space_i… forced_choice_p… extra… /home/jti… 1-2         
#> 15 verbs-in-space verbs_space_m… forced_choice_p… ambig… /home/jti… 4           
#> # ℹ 2 more variables: instruction_text_verbatim <chr>, notes <chr>
```

## Interactive view

Use the search box to filter by dataset, instruction type/status, or
keywords in the verbatim instruction text.

``` r

DT::datatable(
  instruction_tbl,
  rownames = FALSE,
  filter = "top",
  options = list(
    pageLength = 15,
    autoWidth = TRUE,
    scrollX = TRUE
  )
)
```
