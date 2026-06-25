# lexis

`lexis` is an R data package that collates English psycholinguistic rating norms from multiple published sources into a common word-level resource, cleaning source files, standardizing word identifiers, harmonizing dimension names and scale metadata, and aggregating source-specific measures into long and wide tables. `lexis_long` holds rater-averaged rating norms only — each with per-item standard deviations and rater counts — while lexical covariates (lexical-decision and naming latencies, word frequencies, WordNet sense counts) sit alongside in `lexis_wide`. GloVe word embeddings are provided separately as `glove2014` and `glove2024`. Per-study participant details are recorded in `lexis_datasets`, and citations and construct notes are kept beside the data.

## Install

`lexis` is not on CRAN. Install from GitHub:

From GitHub:

```r
remotes::install_github("jaytimm/lexis")
```

---

## Datasets

[![kuperman_2012](https://img.shields.io/static/v1?label=&message=kuperman_2012&color=eeeeee)](#age-of-acquisition) [![temporal](https://img.shields.io/static/v1?label=&message=temporal&color=e2d175)](#) [![n=30121](https://img.shields.io/static/v1?label=n&message=30%2C121&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–25+years&color=dfe6e9)](#)

[![brysbaert_2014](https://img.shields.io/static/v1?label=&message=brysbaert_2014&color=eeeeee)](#concreteness) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=39954](https://img.shields.io/static/v1?label=n&message=39%2C954&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–5&color=dfe6e9)](#)

[![warriner_2013](https://img.shields.io/static/v1?label=&message=warriner_2013&color=eeeeee)](#valence-arousal-dominance) [![affective](https://img.shields.io/static/v1?label=&message=affective&color=ff9f43)](#) [![n=13915](https://img.shields.io/static/v1?label=n&message=13%2C915&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–9+SAM&color=dfe6e9)](#)

[![lynott_2020](https://img.shields.io/static/v1?label=&message=lynott_2020&color=eeeeee)](#lancaster-sensorimotor-norms) [![sensorimotor](https://img.shields.io/static/v1?label=&message=sensorimotor&color=00d2d3)](#) [![n=39707](https://img.shields.io/static/v1?label=n&message=39%2C707&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=0–5&color=dfe6e9)](#) [![dims=11](https://img.shields.io/static/v1?label=modalities&message=11&color=dfe6e9)](#)

[![balota_2007](https://img.shields.io/static/v1?label=&message=balota_2007&color=eeeeee)](#english-lexicon-project) [![covariate](https://img.shields.io/static/v1?label=&message=covariate&color=95a5a6)](#) [![n=40481](https://img.shields.io/static/v1?label=n&message=40%2C481&color=dfe6e9)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=RT+ms&color=dfe6e9)](#)

[![brysbaert_2019](https://img.shields.io/static/v1?label=&message=brysbaert_2019&color=eeeeee)](#word-frequency) [![covariate](https://img.shields.io/static/v1?label=&message=covariate&color=95a5a6)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=Zipf+freq&color=dfe6e9)](#)

[![pexman_2019](https://img.shields.io/static/v1?label=&message=pexman_2019&color=eeeeee)](#body-object-interaction) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=9348](https://img.shields.io/static/v1?label=n&message=9%2C348&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7&color=dfe6e9)](#)

[![diveica_2023](https://img.shields.io/static/v1?label=&message=diveica_2023&color=eeeeee)](#socialness) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=8388](https://img.shields.io/static/v1?label=n&message=8%2C388&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7&color=dfe6e9)](#)

[![engelthaler_2018](https://img.shields.io/static/v1?label=&message=engelthaler_2018&color=eeeeee)](#humor) [![affective](https://img.shields.io/static/v1?label=&message=affective&color=ff9f43)](#) [![n=4997](https://img.shields.io/static/v1?label=n&message=4%2C997&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–5&color=dfe6e9)](#)

[![roberts_2019](https://img.shields.io/static/v1?label=&message=roberts_2019&color=eeeeee)](#gender) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=701](https://img.shields.io/static/v1?label=n&message=701&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7+masc–fem&color=dfe6e9)](#)

[![winter_2023](https://img.shields.io/static/v1?label=&message=winter_2023&color=eeeeee)](#iconicity) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=14776](https://img.shields.io/static/v1?label=n&message=14%2C776&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7&color=dfe6e9)](#)

[![scott_2019](https://img.shields.io/static/v1?label=&message=scott_2019&color=eeeeee)](#glasgow-norms) [![multi](https://img.shields.io/static/v1?label=&message=multi-dimension&color=fd79a8)](#) [![n=5553](https://img.shields.io/static/v1?label=n&message=5%2C553&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7+%2F+1–9&color=dfe6e9)](#) [![dims=9](https://img.shields.io/static/v1?label=dimensions&message=9&color=dfe6e9)](#) [![UK](https://img.shields.io/static/v1?label=raters&message=UK&color=dfe6e9)](#)

---

## Norms and sources

Each column in `lexis_wide` (and each `dimension` value in `lexis_long`) has a corresponding row in `lexis_meta` with the full construct text, scale, original source column, SD availability, and citation. The notes below combine the main construct descriptions with their source datasets.

### Age of Acquisition
Dimension: `aoa`

Construct: estimated age, in years, when a speaker would first have understood the word if someone used it in front of them; linked to lexical fluency.

Scale: 1–25 years. SD available.

Source: Kuperman, V., Stadthagen-Gonzalez, H., & Brysbaert, M. (2012). Age-of-acquisition ratings for 30,000 English words. *Behavior Research Methods*, 44, 978–990.

---

### Concreteness
Dimension: `concreteness`

Construct: degree to which meaning is grounded in direct perception and action versus abstract, definition-only knowledge.

Scale: 1–5, abstract to concrete. SD available.

Source: Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. *Frontiers in Psychology*, 5, 1515.

---

### Valence, Arousal, Dominance
Dimensions:

| Dimension | Construct |
|---|---|
| `valence` | Pleasantness of the feeling evoked by the word. |
| `arousal` | Intensity or activation of the feeling evoked by the word. |
| `dominance` | Sense of control or submissiveness implied by the word’s meaning. |

Scale: 1–9 Self-Assessment Manikin. SD available.

Source: Warriner, A. B., Kuperman, V., & Brysbaert, M. (2013). Norms of valence, arousal, and dominance for 13,915 English lemmas. *Behavior Research Methods*, 45, 1191–1207.

---

### Lancaster Sensorimotor Norms
Dimensions:

| Dimension | Construct |
|---|---|
| `lancaster_auditory` | Strength of experiencing the concept through hearing. |
| `lancaster_gustatory` | Strength of experiencing the concept through taste. |
| `lancaster_haptic` | Strength of experiencing the concept through touch. |
| `lancaster_interoceptive` | Strength of internal bodily sensations tied to the concept. |
| `lancaster_olfactory` | Strength of experiencing the concept through smell. |
| `lancaster_visual` | Strength of experiencing the concept through sight. |
| `lancaster_foot_leg` | Strength of foot/leg involvement in actions tied to the concept. |
| `lancaster_hand_arm` | Strength of hand/arm involvement in actions tied to the concept. |
| `lancaster_head` | Strength of head involvement, excluding mouth, in actions tied to the concept. |
| `lancaster_mouth` | Strength of mouth/throat involvement in actions tied to the concept. |
| `lancaster_torso` | Strength of torso involvement in actions tied to the concept. |

Scale: 0–5 strength of perceptual/motor association. SD available.

Source: Lynott, D., Connell, L., Brysbaert, M., Brand, J., & Carney, J. (2020). The Lancaster sensorimotor norms. *Behavior Research Methods*, 52, 1271–1291.

---

### Body-Object Interaction
Dimension: `boi`

Construct: how easily a person can physically interact with the object or concept using their hands or body.

Scale: 1–7. SD available.

Source: Pexman, P. M., Muraki, E., Sidhu, D. M., Siakaluk, P. D., & Yap, M. J. (2019). Ratings for 8,000 English words. *Behavior Research Methods*, 51, 1134–1146.

---

### Socialness
Dimension: `socialness`

Construct: how much the meaning concerns people, interaction, roles, institutions, or social ideas.

Scale: 1–7. SD available.

Source: Diveica, V., Pexman, P. M., & Binney, R. J. (2023). Quantifying social semantics. *Behavior Research Methods*, 55, 461–473.

---

### Humor
Dimension: `humor`

Construct: how funny or humor-associated the word feels; largely distinct from valence, arousal, and concreteness.

Scale: 1–5. SD available.

Source: Engelthaler, T., & Hills, T. T. (2018). Humor norms for 4,997 English words. *Behavior Research Methods*, 50, 1116–1124.

---

### Gender
Dimension: `gender_femininity`

Construct: perceived masculine versus feminine association of the word’s meaning in discourse.

Scale: 1–7, masculine to feminine. SD available.

Source: Roberts, B. E., & Utych, S. M. (2019). Linking gender and language: Developing a database of masculine and feminine words. *American Politics Research*, 47, 1155–1173.

---

### Iconicity
Dimension: `iconicity`

Construct: degree to which a word's sound or form resembles or evokes its meaning (sound symbolism); rated by naive speakers without prior knowledge of the concept.

Scale: 1–7, arbitrary to iconic. SD available.

Source: Winter, B., Lupyan, G., Perry, L. K., Dingemanse, M., & Perlman, M. (2023). Iconicity ratings for 14,000+ English words. *Behavior Research Methods*.

---

### Glasgow Norms
Dimensions (all prefixed `glasgow_`, from a single UK rater pool):

| Dimension | Construct |
|---|---|
| `glasgow_valence` | Pleasantness of the feeling evoked by the word. |
| `glasgow_arousal` | Intensity or activation of the feeling evoked by the word. |
| `glasgow_dominance` | Sense of control or dominance implied by the word. |
| `glasgow_concreteness` | Concrete (perceptible) versus abstract meaning. |
| `glasgow_imageability` | Ease of evoking a sensory mental image. |
| `glasgow_familiarity` | Subjective familiarity with the word. |
| `glasgow_aoa` | Subjective age of acquisition as a 1–7 band rating (not years). |
| `glasgow_size` | Perceived physical size of the referent. |
| `glasgow_gender` | Gender association, 1 = feminine to 7 = masculine. |

Scale: valence, arousal, and dominance are 1–9; the rest are 1–7. SD and N available for all. Dimensions are kept under a `glasgow_` prefix — rather than merged into the US-rated `valence`, `concreteness`, etc. — because the ratings come from a UK (University of Glasgow) pool; see `lexis_datasets$rater_population` to filter or compare across populations.

Source: Scott, G. G., Keitel, A., Becirspahic, M., Yao, B., & Sereno, S. C. (2019). The Glasgow Norms: Ratings of 5,500 words on nine scales. *Behavior Research Methods*, 51, 1258–1270.

---

### Lexical covariates

These columns are **not rating norms** — they carry no rater-averaged judgment, per-item SD, or rater count — and appear in `lexis_wide` only, joined onto the norm vocabulary. They are useful as predictors and stratifiers.

| Column | Construct | Source |
|---|---|---|
| `lexdec_rt` | Mean lexical-decision time (ms): word vs. nonword judgment; reflects accessibility. | Balota et al. (2007), English Lexicon Project |
| `lexdec_naming_rt` | Mean naming latency (ms): reading the word aloud; sensitive to phonology/articulation. | Balota et al. (2007) |
| `freq_zipf_us` | SUBTLEX-US Zipf word frequency. | Brysbaert et al. (2019) |
| `wf_zipf` | Zipf word frequency aggregated across Wikipedia, subtitles, news, web, and social media. | Speer (2023), `wordfreq` v3.1.1 |
| `wn_n_synsets` | Number of WordNet synsets (a polysemy measure). | WordNet 3.1 (Miller, 1995) |
| `wn_n_noun`, `wn_n_verb`, `wn_n_adj`, `wn_n_adv` | Synset counts by part of speech; non-zero counts give grammatical versatility. | WordNet 3.1 |

---

### GloVe Embeddings
Dimensions: dense 300-dimensional semantic vectors (`glove2014`, `glove2024`) trained on large text corpora and used for distributional semantic similarity.

Scale: continuous numeric vector components (300d).

Source: Pennington, J., Socher, R., & Manning, C. D. (2014). GloVe: Global vectors for word representation. Proceedings of the 2014 Conference on Empirical Methods in Natural Language Processing (EMNLP), 1532-1543. 

Carlson, R., Bauer, J., & Manning, C. D. (2025). A New Pair of GloVes. arXiv:2507.18103. [https://arxiv.org/abs/2507.18103](https://arxiv.org/abs/2507.18103). Project page: [Stanford GloVe](https://nlp.stanford.edu/projects/glove/).

---

## Data objects

| Object | Description |
|---|---|
| `lexis_long` | Long-format table of rater-averaged rating norms: one row per word–dimension observation. Columns: `word`, `lemma`, `dataset`, `dimension`, `mean`, `sd`, `n_ratings`, `scale_min`, `scale_max`. |
| `lexis_wide` | Wide-format table: one row per word (vocabulary defined by `lexis_long`), one column per norm dimension, plus appended lexical-covariate columns (lexical-decision/naming RTs, word frequencies, WordNet sense counts). |
| `lexis_meta` | Metadata table: one row per rating-norm dimension, with construct description, scale, source column, SD availability, and citation. |
| `lexis_datasets` | Per-study provenance: one row per source study, with citation, word/participant counts, recruitment platform, age, country, and rater population. |
| `norming_instructions` | Per-construct verbatim and LLM-adapted participant instructions, compiled from `data-raw/_annotations/`. |
| `glove2014` | Named numeric matrix: 300-dimensional GloVe embeddings (Pennington et al., 2014) subset to the lexis vocabulary. |
| `glove2024` | Named numeric matrix: 300-dimensional GloVe embeddings (Carlson et al., 2025) subset to the lexis vocabulary. |
