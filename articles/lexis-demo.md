# lexis Demo

`lexis` provides word-level psycholinguistic norms and helper functions
for lookup, filtering, definitions, embeddings, and matching.

## Load package

``` r

library(lexis)
```

## Quick lookup

Use `lex_lookup()` to retrieve values for one or more words.

``` r

lex_lookup(c("dog", "justice"), dims = c("aoa", "concreteness", "valence"))
#> # A tibble: 6 × 9
#>   word    lemma   dataset    dimension  mean    sd n_ratings scale_min scale_max
#>   <chr>   <chr>   <chr>      <chr>     <dbl> <dbl>     <int>     <dbl>     <dbl>
#> 1 dog     dog     aoa        aoa        2.8   1.2         20         1        25
#> 2 justice justice aoa        aoa        9.47  2.76        19         1        25
#> 3 dog     dog     concreten… concrete…  4.85  0.46        27         1         5
#> 4 justice justice concreten… concrete…  1.45  0.74        29         1         5
#> 5 dog     dog     warriner_… valence    7     2.07        42         1         9
#> 6 justice justice warriner_… valence    6.8   1.54        41         1         9
```

Use `wide = TRUE` for one row per word.

``` r

lex_lookup(c("dog", "justice"), wide = TRUE)
#> # A tibble: 2 × 30
#>   word    lemma     aoa   boi concreteness gender_femininity humor
#>   <chr>   <chr>   <dbl> <dbl>        <dbl>             <dbl> <dbl>
#> 1 dog     dog      2.8    6.4         4.85              5.22 NA   
#> 2 justice justice  9.47  NA           1.45              4.87  1.93
#> # ℹ 23 more variables: lancaster_auditory <dbl>, lancaster_gustatory <dbl>,
#> #   lancaster_haptic <dbl>, lancaster_interoceptive <dbl>,
#> #   lancaster_olfactory <dbl>, lancaster_visual <dbl>,
#> #   lancaster_foot_leg <dbl>, lancaster_hand_arm <dbl>, lancaster_head <dbl>,
#> #   lancaster_mouth <dbl>, lancaster_torso <dbl>, lexdec_rt <dbl>,
#> #   lexdec_naming_rt <dbl>, prevalence_pknown <dbl>, prevalence_score <dbl>,
#> #   prevalence_freq_zipf <dbl>, ser <dbl>, socialness <dbl>, valence <dbl>, …
```

## Search and filter

`lex_search()` helps find words by pattern.

``` r

lex_search("^bio", cols = "word")
#> # A tibble: 164 × 2
#>    word         lemma       
#>    <chr>        <chr>       
#>  1 biological   biological  
#>  2 biology      biology     
#>  3 bio          bio         
#>  4 biopsy       biopsy      
#>  5 biography    biography   
#>  6 biologist    biologist   
#>  7 biochemical  biochemical 
#>  8 bionic       bionic      
#>  9 biochemistry biochemistry
#> 10 biographer   biographer  
#> # ℹ 154 more rows
```

`lex_filter()` filters on numeric dimensions.

``` r

lex_filter(concreteness > 4, aoa < 8)
#> # A tibble: 2,515 × 75
#>    word   lemma    aoa   boi concreteness gender_femininity humor imageability
#>    <chr>  <chr>  <dbl> <dbl>        <dbl>             <dbl> <dbl>        <dbl>
#>  1 can    can     4.32  4.88         4.55             NA    NA             416
#>  2 man    man     3.11  5.35         4.79              6.4   2.10          620
#>  3 people people  3.52  6.08         4.82              4.19  2.19          568
#>  4 night  night   3.61  2.13         4.52             NA     1.92           NA
#>  5 talk   talk    3.68  4.07         4.07             NA    NA             467
#>  6 guy    guy     4.11  5.84         4.68              5.85  2.47           NA
#>  7 money  money   5.11  6.29         4.54              4.63  2.25           NA
#>  8 girl   girl    4     5.52         4.85             NA     2.65           NA
#>  9 father father  4.11  6.04         4.52             NA     2.44           NA
#> 10 boy    boy     3.67  4.96         4.76             NA     2.59           NA
#> # ℹ 2,505 more rows
#> # ℹ 67 more variables: lancaster_auditory <dbl>, lancaster_gustatory <dbl>,
#> #   lancaster_haptic <dbl>, lancaster_interoceptive <dbl>,
#> #   lancaster_olfactory <dbl>, lancaster_visual <dbl>,
#> #   lancaster_foot_leg <dbl>, lancaster_hand_arm <dbl>, lancaster_head <dbl>,
#> #   lancaster_mouth <dbl>, lancaster_torso <dbl>, lexdec_rt <dbl>,
#> #   lexdec_naming_rt <dbl>, prevalence_pknown <dbl>, prevalence_score <dbl>, …
```

## Dictionary definitions

Use `lex_define()` to pull Wordset definitions.

``` r

lex_define(c("bank", "charge"), n = 3)
#> # A tibble: 12 × 6
#>    word   speech_part def                                example synonyms labels
#>    <chr>  <chr>       <chr>                              <chr>   <chr>    <chr> 
#>  1 bank   noun        a flight maneuver                   NA     slip     NA    
#>  2 bank   noun        a building in which the business … "the b… bank bu… NA    
#>  3 bank   noun        a container, usually with a slot … "I use… money b… NA    
#>  4 bank   verb        to have confidence or faith in     "I ban… trust; … NA    
#>  5 bank   verb        to cover with ashes so to control… "The f… NA       trans…
#>  6 bank   verb        to enclose with a bank             "The h… NA       trans…
#>  7 charge noun        a special assignment that is give…  NA     mission  NA    
#>  8 charge noun        attention and management implying…  NA     care     NA    
#>  9 charge noun        an impetuous rush toward someone … "The b… NA       NA    
#> 10 charge verb        to saturate                        "The r… NA       trans…
#> 11 charge verb        to energize a battery by passing … "I nee… NA       NA    
#> 12 charge verb        to cause formation of a net elect… "charg… NA       NA
```

## Embeddings

Get vectors with `lex_embed()`.

``` r

emb <- lex_embed(c("dog", "cat", "wolf"))
dim(emb)
#> [1]  3 50
```

Nearest neighbors with `lex_neighbors()`.

``` r

lex_neighbors("dog", n = 10)
#> # A tibble: 10 × 2
#>    word   similarity
#>    <chr>       <dbl>
#>  1 cat         0.941
#>  2 dogs        0.873
#>  3 boy         0.845
#>  4 pet         0.821
#>  5 rabbit      0.820
#>  6 baby        0.820
#>  7 horse       0.804
#>  8 puppy       0.801
#>  9 mad         0.793
#> 10 monkey      0.787
```

## Stimulus matching

`lex_match()` finds words matched on selected dimensions.

``` r

lex_match(
  targets = c("anger", "joy"),
  dims = c("aoa", "concreteness", "valence"),
  n = 3
)
#> # A tibble: 6 × 6
#>   target match         distance   aoa concreteness valence
#>   <chr>  <chr>            <dbl> <dbl>        <dbl>   <dbl>
#> 1 joy    excited          0.221  6.21         2.32    8.11
#> 2 joy    peaceful         0.263  7.22         2.27    8   
#> 3 joy    joyful           0.266  7.05         2.12    8.21
#> 4 anger  emergency        0.173  5.94         2.41    2.72
#> 5 anger  frightened       0.193  5.65         2.27    2.45
#> 6 anger  embarrassment    0.203  6.28         2.43    2.72
```
