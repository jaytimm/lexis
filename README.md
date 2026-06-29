# lexis

`lexis` is an R data package that brings together English psycholinguistic norms from published sources into a harmonized word-level resource. It includes rater-averaged ratings with uncertainty and sample-size information, lexical covariates such as reaction times, word frequencies, and WordNet sense counts, plus study metadata, construct notes, citations, and GloVe word embeddings.

## Install

`lexis` is not on CRAN. Install from GitHub:

From GitHub:

```r
remotes::install_github("jaytimm/lexis")
```

---

## Datasets

- Age of acquisition (Kuperman et al., 2012)
- Concreteness (Brysbaert et al., 2014)
- Valence, arousal & dominance (Warriner et al., 2013)
- Lancaster sensorimotor norms, 11 modalities (Lynott et al., 2020)
- English Lexicon Project — lexical-decision & naming RTs (Balota et al., 2007)
- Word frequency, SUBTLEX-US (Brysbaert et al., 2019)
- Body–object interaction (Pexman et al., 2019)
- Socialness (Diveica et al., 2023)
- Humor (Engelthaler & Hills, 2018)
- Iconicity (Winter et al., 2023)
- Glasgow Norms, 9 dimensions (Scott et al., 2019)

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
| `subtlex_us_zipf` | SUBTLEX-US Zipf word frequency (US film-subtitle corpus). | Brysbaert et al. (2019) |
| `wordfreq_en_zipf` | Zipf word frequency, general-English blend (Wikipedia, subtitles, news, web, social media). | Speer (2023), `wordfreq` v3.1.1 |
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
