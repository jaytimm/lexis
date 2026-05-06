# lexis

`lexis` is an R data package that collates English psycholinguistic norms from multiple published sources into a common word-level resource, cleaning source files, standardizing word identifiers, harmonizing dimension names and scale metadata, and aggregating source-specific measures into tidy long and wide tables. The package contains a large union vocabulary across norming studies, dictionary entries, and GloVe word embeddings, with citations and construct notes kept alongside the data.

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

[![balota_2007](https://img.shields.io/static/v1?label=&message=balota_2007&color=eeeeee)](#english-lexicon-project) [![lexical](https://img.shields.io/static/v1?label=&message=lexical&color=5f27cd)](#) [![n=40481](https://img.shields.io/static/v1?label=n&message=40%2C481&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=RT+ms&color=dfe6e9)](#)

[![brysbaert_2019](https://img.shields.io/static/v1?label=&message=brysbaert_2019&color=eeeeee)](#word-prevalence) [![lexical](https://img.shields.io/static/v1?label=&message=lexical&color=5f27cd)](#) [![n=61855](https://img.shields.io/static/v1?label=n&message=61%2C855&color=dfe6e9)](#) [![no-sd](https://img.shields.io/static/v1?label=SD&message=unavailable&color=ee5a24)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=proportion&color=dfe6e9)](#)

[![pexman_2019](https://img.shields.io/static/v1?label=&message=pexman_2019&color=eeeeee)](#body-object-interaction) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=9348](https://img.shields.io/static/v1?label=n&message=9%2C348&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7&color=dfe6e9)](#)

[![diveica_2023](https://img.shields.io/static/v1?label=&message=diveica_2023&color=eeeeee)](#socialness) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=8388](https://img.shields.io/static/v1?label=n&message=8%2C388&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7&color=dfe6e9)](#)

[![juhasz_2013](https://img.shields.io/static/v1?label=&message=juhasz_2013&color=eeeeee)](#sensory-experience-rating) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=5857](https://img.shields.io/static/v1?label=n&message=5%2C857&color=dfe6e9)](#) [![no-sd](https://img.shields.io/static/v1?label=SD&message=unavailable&color=ee5a24)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7&color=dfe6e9)](#)

[![engelthaler_2018](https://img.shields.io/static/v1?label=&message=engelthaler_2018&color=eeeeee)](#humor) [![affective](https://img.shields.io/static/v1?label=&message=affective&color=ff9f43)](#) [![n=4997](https://img.shields.io/static/v1?label=n&message=4%2C997&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–5&color=dfe6e9)](#)

[![bird_2001](https://img.shields.io/static/v1?label=&message=bird_2001&color=eeeeee)](#imageability) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=2019](https://img.shields.io/static/v1?label=n&message=2%2C019&color=dfe6e9)](#) [![no-sd](https://img.shields.io/static/v1?label=SD&message=unavailable&color=ee5a24)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=100–700+MRC&color=dfe6e9)](#)

[![roberts_2019](https://img.shields.io/static/v1?label=&message=roberts_2019&color=eeeeee)](#gender) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=701](https://img.shields.io/static/v1?label=n&message=701&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7+masc–fem&color=dfe6e9)](#)

[![verbs_in_space](https://img.shields.io/static/v1?label=&message=verbs_in_space&color=eeeeee)](#verbs-in-space) [![spatial](https://img.shields.io/static/v1?label=&message=spatial&color=baee90)](#) [![n=299](https://img.shields.io/static/v1?label=n&message=299&color=dfe6e9)](#) [![no-sd](https://img.shields.io/static/v1?label=SD&message=unavailable&color=ee5a24)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=continuous&color=dfe6e9)](#) [![dims=8](https://img.shields.io/static/v1?label=directions&message=8&color=dfe6e9)](#)

[![winter_2023](https://img.shields.io/static/v1?label=&message=winter_2023&color=eeeeee)](#iconicity) [![semantic](https://img.shields.io/static/v1?label=&message=semantic&color=54a0ff)](#) [![n=14776](https://img.shields.io/static/v1?label=n&message=14%2C776&color=dfe6e9)](#) [![sd](https://img.shields.io/static/v1?label=SD&message=available&color=2ecc71)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=1–7&color=dfe6e9)](#)

[![wordset](https://img.shields.io/static/v1?label=&message=wordset&color=eeeeee)](#wordset-dictionary) [![dictionary](https://img.shields.io/static/v1?label=&message=dictionary&color=dddddd)](#) [![n=108140](https://img.shields.io/static/v1?label=n&message=108%2C140&color=dfe6e9)](#) [![scale](https://img.shields.io/static/v1?label=scale&message=count&color=dfe6e9)](#)

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

### English Lexicon Project
Dimensions:

| Dimension | Construct |
|---|---|
| `lexdec_rt` | Mean lexical-decision time in ms: word versus nonword judgment; reflects accessibility. |
| `lexdec_naming_rt` | Mean naming latency in ms: reading the word aloud; sensitive to phonology and articulation. |

Scale: reaction time in milliseconds. SD available.

Source: Balota, D. A., et al. (2007). The English Lexicon Project. *Behavior Research Methods*, 39, 445–459.

---

### Word Prevalence
Dimensions:

| Dimension | Construct |
|---|---|
| `prevalence_pknown` | Estimated proportion of speakers who know the word, based on vocabulary-test performance. |
| `prevalence_score` | Probit-style continuous prevalence score; 0 is approximately known by half of speakers. |

Scale: `prevalence_pknown` is 0–1; `prevalence_score` is unbounded. SD unavailable.

Source: Brysbaert, M., Mandera, P., McCormick, S. F., & Keuleers, E. (2019). Word prevalence norms for 62,000 English lemmas. *Behavior Research Methods*, 51, 1583–1603.

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

### Sensory Experience Rating
Dimension: `ser`

Construct: how strongly reading the word evokes sensory experience across modalities.

Scale: 1–7. SD unavailable.

Source: Juhasz, B. J., & Yap, M. J. (2013). Sensory experience ratings for over 5,000 mono- and disyllabic words. *Behavior Research Methods*, 45, 111–120.

---

### Humor
Dimension: `humor`

Construct: how funny or humor-associated the word feels; largely distinct from valence, arousal, and concreteness.

Scale: 1–5. SD available.

Source: Engelthaler, T., & Hills, T. T. (2018). Humor norms for 4,997 English words. *Behavior Research Methods*, 50, 1116–1124.

---

### Imageability
Dimension: `imageability`

Construct: ease of bringing a vivid sensory mental image to mind.

Scale: transformed 100–700 imageability score, aligned to the MRC Psycholinguistic Database metric from original 1–7 ratings. SD unavailable.

Source: Bird, H., Franklin, S., & Howard, D. (2001). Age of acquisition and imageability ratings for a large set of words, including verbs and function words. *Behavior Research Methods, Instruments, & Computers*, 33(1), 73–79.

---

### Gender
Dimension: `gender_femininity`

Construct: perceived masculine versus feminine association of the word’s meaning in discourse.

Scale: 1–7, masculine to feminine. SD available.

Source: Roberts, B. E., & Utych, S. M. (2019). Linking gender and language: Developing a database of masculine and feminine words. *American Politics Research*, 47, 1155–1173.

---

### Verbs in Space
Dimensions:

| Dimension | Construct |
|---|---|
| `vis_upwrd` | Verbs: composite association of the action with upward motion. |
| `vis_dwnwrd` | Verbs: composite association with downward motion. |
| `vis_vert` | Verbs: composite vertical spatial association. |
| `vis_left` | Verbs: composite association with leftward motion. |
| `vis_right` | Verbs: composite association with rightward motion. |
| `vis_horiz` | Verbs: composite horizontal spatial association. |
| `vis_toward` | Verbs: composite association with motion toward a reference point. |
| `vis_away` | Verbs: composite association with motion away from a reference point. |

Scale: continuous directional association scores. SD unavailable.

Source: Meteyard, L., & Vigliocco, G. (2009). Verbs in space: Axis and direction of motion norms for 299 English verbs. *Behavior Research Methods*, 41(2), 565–574.

---

### Iconicity
Dimension: `iconicity`

Construct: degree to which a word's sound or form resembles or evokes its meaning (sound symbolism); rated by naive speakers without prior knowledge of the concept.

Scale: 1–7, arbitrary to iconic. SD available.

Source: Winter, B., Lupyan, G., Perry, L. K., Dingemanse, M., & Perlman, M. (2023). Iconicity ratings for 14,000+ English words. *Behavior Research Methods*.

---

### Wordset Dictionary
Derived dimensions:

| Dimension | Construct |
|---|---|
| `n_defs` | Total definition count across POS; an indicator of semantic richness / polysemy. |
| `n_pos` | Number of distinct parts of speech listed for the word. |

Scale: counts. Source: [wordset/wordset-dictionary](https://github.com/wordset/wordset-dictionary), open English dictionary in JSON format. `ws_pos` is included in wide format as a semicolon-separated POS reference list. Because most norming studies present words without sentence context or explicit sense disambiguation, Wordset POS entries should not be interpreted as identifying the rated sense.

---

### GloVe Embeddings
Dimensions: dense semantic vectors (`glove50`, `glove300`) trained on large text corpora and used for distributional semantic similarity.

Scale: continuous numeric vector components (50d and 300d).

Source: Pennington, J., Socher, R., & Manning, C. D. (2014). GloVe: Global vectors for word representation. Proceedings of the 2014 Conference on Empirical Methods in Natural Language Processing (EMNLP), 1532-1543. 

Carlson, R., Bauer, J., & Manning, C. D. (2025). A New Pair of GloVes. arXiv:2507.18103. [https://arxiv.org/abs/2507.18103](https://arxiv.org/abs/2507.18103). Project page: [Stanford GloVe](https://nlp.stanford.edu/projects/glove/).

---

## Data objects

`lexis` provides `lexis_long`, `lexis_wide`, `lexis_meta`, `glove50`, and `wordset_dict`.

| Object | Description |
|---|---|
| `lexis_long` | Tidy long-format table: one row per word–dimension observation. Columns: `word`, `lemma`, `dataset`, `dimension`, `mean`, `sd`, `n_ratings`, `scale_min`, `scale_max`. |
| `lexis_wide` | Wide-format table: one row per word, one column per dimension. Includes supplementary columns from source datasets. |
| `lexis_meta` | Metadata table: one row per dimension, with construct description, scale, source column, SD availability, and citation. |
| `glove50` | Named numeric matrix (145,670 × 50): 2024 GloVe embeddings trained on Wikipedia + Gigaword. Row names are word forms. |
| `glove300` | Named numeric matrix (145,670 × 300): 2024 GloVe embeddings, 300-dimensional. Not tracked in git; rebuild with `data-raw/build_glove.R`. |
| `wordset_dict` | Wordset dictionary entries with definitions, POS, and examples. |

---

## Instructions Table

These prompts are standardized for LLM use: each construct has a concise instruction, consistent response format, and explicit scale anchors derived from the original study instructions.

| construct-name | scale-anchors | instructions-llm |
|---|---|---|
| age_of_acquisition | {"1": "learned very early", "25": "learned very late"} | Estimate the age in years at which a typical English speaker first understands this word. Range: 1-25 (1 = learned very early, 25 = learned very late). Reply with one number. |
| arousal | {"1": "calm", "9": "excited"} | Rate the emotional arousal of this word. Range: 1-9 (1 = calm, 9 = excited). Reply with one number. |
| body_object_interaction | {"1": "low body-object interaction", "7": "high body-object interaction"} | Rate how easily a human body can physically interact with what this word refers to. Range: 1-7 (1 = low interaction, 7 = high interaction). Reply with one number. |
| concreteness | {"1": "Abstract (language based)", "5": "Concrete (experience based)"} | Rate how concrete this word's meaning is. Concrete words refer to things experienced through the senses; abstract words refer to meanings defined through other words. Range: 1-5 (1 = abstract, 5 = concrete). Reply with one number. |
| dominance | {"1": "controlled", "9": "in control"} | Rate the sense of control this word evokes. Range: 1-9 (1 = controlled/submissive, 9 = in control/dominant). Reply with one number. |
| gendered_word_association | {"1": "very feminine", "7": "very masculine"} | Rate the gender association of this word. Range: 1-7 (1 = very feminine, 7 = very masculine). Reply with one number. |
| humor | {"1": "humorless = not funny at all", "5": "humorous = most funny"} | Rate how funny this word feels on first reading. Range: 1-5 (1 = not funny at all, 5 = very funny). Reply with one number. |
| iconicity | {"1": "Not iconic at all", "7": "Very iconic"} | Rate how much this word sounds like what it means - an iconic word is one whose meaning you might guess without knowing English. Range: 1-7 (1 = not iconic at all, 7 = very iconic). Reply with one number. |
| imageability | {"1": "least imageable", "7": "most imageable"} | Rate how easily this word evokes a mental image. Range: 1-7 (1 = least imageable, 7 = most imageable). Reply with one number. |
| lancaster_auditory | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced through hearing. Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_foot_leg | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced by performing actions with the foot or leg. Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_gustatory | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced through taste. Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_hand_arm | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced by performing actions with the hand or arm. Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_haptic | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced through touch (feeling with the skin or body). Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_head | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced by performing actions with the head, excluding the mouth (e.g., nodding, turning, tilting). Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_interoceptive | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced through sensations inside the body (e.g., hunger, pain, heartbeat, fatigue). Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_mouth | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced by performing actions with the mouth or throat (e.g., speaking, chewing, swallowing). Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_olfactory | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced through smell. Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_torso | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced by performing actions with the torso (e.g., bending, twisting, posture). Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| lancaster_visual | {"0": "not experienced at all", "5": "experienced greatly"} | Rate how strongly the meaning of this word is experienced through vision. Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number. |
| sensory_experience_rating | {"1": "no sensory experience", "7": "strong sensory experience"} | Rate how strongly this word evokes any sensory experience. Range: 1-7 (1 = no sensory experience, 7 = strong sensory experience). Reply with one number. |
| socialness | {"1": "low social relevance", "7": "high social relevance"} | Rate how socially relevant this word's meaning is - does it refer to social characteristics, behaviors, roles, spaces, institutions, values, or interactions? Range: 1-7 (1 = not socially relevant, 7 = highly socially relevant). Reply with one number. |
| valence | {"1": "unhappy", "9": "happy"} | Rate the emotional valence of this word. Range: 1-9 (1 = very unhappy/negative, 9 = very happy/positive). Reply with one number. |
| vis_away | {"1": "weak association", "5": "strong association"} | Rate the strength of association between this verb and away motion in space. Range: 1-5 (1 = weak association, 5 = strong association). Reply with one number. |
| vis_dwnwrd | {"1": "weak association", "5": "strong association"} | Rate the strength of association between this verb and downward motion in space. Range: 1-5 (1 = weak association, 5 = strong association). Reply with one number. |
| vis_horiz | {"1": "weak association", "5": "strong association"} | Rate the strength of association between this verb and horizontal motion in space. Range: 1-5 (1 = weak association, 5 = strong association). Reply with one number. |
| vis_left | {"1": "weak association", "5": "strong association"} | Rate the strength of association between this verb and left motion in space. Range: 1-5 (1 = weak association, 5 = strong association). Reply with one number. |
| vis_right | {"1": "weak association", "5": "strong association"} | Rate the strength of association between this verb and right motion in space. Range: 1-5 (1 = weak association, 5 = strong association). Reply with one number. |
| vis_toward | {"1": "weak association", "5": "strong association"} | Rate the strength of association between this verb and toward motion in space. Range: 1-5 (1 = weak association, 5 = strong association). Reply with one number. |
| vis_upwrd | {"1": "weak association", "5": "strong association"} | Rate the strength of association between this verb and upward motion in space. Range: 1-5 (1 = weak association, 5 = strong association). Reply with one number. |
| vis_vert | {"1": "weak association", "5": "strong association"} | Rate the strength of association between this verb and vertical motion in space. Range: 1-5 (1 = weak association, 5 = strong association). Reply with one number. |
| word_knowledge_proportion_pknown | {"0": "known by none", "1": "known by all"} | Estimate the proportion of English speakers who know this word. Range: 0-1 (0 = almost none, 1 = almost all). Reply with one number. |
