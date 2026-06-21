# Language Models and Embeddings Capture Different Psycholinguistic Constructs: A Multi-Model Benchmark Across 18 Dimensions

**Jay Timm**
*[Institution]*

---

## Abstract (rewritten draft)

Two computational routes now exist for approximating human psycholinguistic norms: prompting large language models (LLMs) for ratings, and supervised regression on word embeddings. The two are rarely compared on the same items, and existing evaluations rest on small convenience samples, so it is not known which constructs each route captures. We benchmark four LLMs spanning size and provider — gpt-4o-mini, gpt-4.1, claude-haiku-4-5, and gpt-5.4-nano — against GloVe-300 and BGE-large-en-v1.5 ridge models on a stratified 1,638-word holdout across 18 dimensions. 

The advantage reversed by construct. [what adavangate??] BGE-large-en-v1.5 was best overall (mean *r* = .734; 13 of 18 dimensions), but all four LLMs led on the same evaluative and experience-linked cluster — valence, arousal, age of acquisition, gustatory, and olfactory — while the embedding advantage concentrated on abstract and sensorimotor items. [Horrible sentence, long -- but/while, em dashes]

Word-level concreteness moderated the gap: the embedding advantage grew as words became more abstract, the same split the dimensions showed.[wtf does this mean: 'the same split the dimensions showed'??] 


[zero segue into this second piece at asll] 
The same boundary appeared inside the LLM response distribution. [What boundary?? no segue here] Its probability-weighted mean tracked human norms better than the standard top-token rating, on all 18 dimensions and on both log-probability-capable models. 

Averaging five to ten temperature-1.0 samples recovered the same value (*r* = .997), so the approach carries over to models that withhold log-probabilities. Its spread tracked human inter-rater disagreement, selectively on semantically rich dimensions. 

For constructs recoverable from language use, the LLM rating distribution was calibrated in both mean and spread. For constructs that depend on direct experience, calibration broke down and the supervised embeddings held the advantage. 

For norm extension, then, the choice of representation should follow the construct, and LLM ratings should be read as distributions rather than single tokens. [2nd piece here just tacked on -- just like it has been the entire abstract]

---

# Introduction (hypothesis-framed draft)

Psycholinguistic norms — ratings of concreteness, valence, sensorimotor grounding, age of acquisition — are aggregated speaker judgments used to investigate how words are represented and processed in the mental lexicon. 

Usage-based accounts hold that lexical knowledge accumulates from experience with language and experience with the world. Part of a word's representation derives from how the speech community uses it, from its contexts, collocates, and evaluative coloring. [citation likely needed here.] That part of lexical knowledge is in principle recoverable from corpora.[citation likely needed here.]  The rest derives from bodily, perceptual, and developmental experience, which text records only secondhand (Barsalou, 1999, 2008; Andrews et al., 2009; Banks et al., 2021; Connell & Lynott, 2024). [second-hand??] 


A speaker rating a word draws on both. 

[better segue]
Two computational routes now compete to approximate [these?] human norms at scale: generative elicitation, in which a large language model (LLM) rates words as a simulated participant (Aher et al., 2023; Stevenson et al., 2022; Trott, 2024), and supervised regression on word embeddings (Recchia & Jones, 2012; Bestgen & Vincze, 2012; Mandera et al., 2017; Utsumi, 2020). 

Hand-collected norms are expensive, so both routes are attractive. [However] But the diagnostic question is not whether either can substitute for human raters. [; instead] It is where each succeeds, where each fails, and what the boundary reveals about which parts of lexical knowledge are recoverable from language use alone.

[better segue] [At least three things have been ...] 
Three things are reasonably well established in the litereature. [wrt LLMs as normers -- not embeddings]

[First]
LLM ratings correlate substantially with human norms across many constructs (Trott, 2024; Kello et al., 2025; Green et al., 2025; Hagihara & Miyazawa, 2026; Peng et al., 2025). 

[Need to list some actual findings here ...]

[Better segue .. eg, As these findings attest ... or something]
The alignment is uneven: stronger for affective and lexical dimensions, weaker for sensorimotor dimensions that require translating language into bodily experience (Trott, 2024; Hagihara & Miyazawa, 2026; Conde, González, et al., 2025; Xu et al., 2025). 

[Incorporate better .. eg, In fact .. or lead with this -?]
This is what grounded-cognition accounts predict if text-trained models recover judgments stabilized in language use but not those that depend on direct perceptual experience (Connell & Lynott, 2024). 

[Segue -- we are not leading a sentences with 'How' -- state the finding then describe it -- wtf --]

[Second ... ] 
How a rating is extracted from the model also matters. A human norm is an expected value — a mean over raters. The standard procedure instead records the single top token at temperature zero, a point estimate. 

For models that expose log-probabilities, taking the probability-weighted mean of the response distribution improves alignment with human norms (Argyle et al., 2023; Martínez et al., 2025; Conde, González, et al., 2025). That evidence comes mostly from large proprietary models, and is thinner for small and local ones. 

Elicitation of either kind is also rarely tested against embedding regression on the same items (cf. Peng et al., 2025).


[This whole section is nonsensical -- and does not read as intro or known at all]

[is this the third known?? does not read like it - ]
The probability-weighting result is usually presented as an extraction trick. 

It is better read as one half of a stronger claim. A published norm is not just a mean: it comes with a standard deviation, because raters disagree, and they disagree more on some words than others. The model's response distribution likewise has a mean and a spread. If that distribution is the computational analogue of a sample of raters — which is what the expected-value argument tacitly assumes — then both moments should be calibrated: the mean to the human mean, and the spread to human disagreement. 

Only the first half of this claim has been tested.

[What is know about embeddings as normer??]


Several things, then, are not known. 

It is not known whether the established correlations, or the sensorimotor gap, survive a lexically balanced test set. Existing evaluations rest on small convenience samples. Hagihara and Miyazawa (2026) test 695 early-acquired CDI words, which are disproportionately concrete, frequent, and easy to rate. Trott (2024) tests 379 ambiguous Glasgow words, enriched for polysemy. Kello et al. (2025) test a 390-word cross-database intersection that favors common, well-studied items. Green et al. (2025) have 11,074 words but evaluate only the early-acquired range of the norm itself. 

The resources these samples stand in for contain 5,500 to 40,000 words, and the vocabulary that norm extension actually targets — low-frequency, late-acquired, abstract — is the vocabulary the samples leave out. 

It is not known whether the sensorimotor gap is an LLM deficiency or half of a pattern in which the advantage reverses by construct; without a competitor scored on the same items, only one side is mapped. 


[Again, this entire line reads tacked on -- need a larger strategy for incorporating this]
It is not known whether the distributional reading survives the loss of log-probability access. The GPT-5 series and all Claude models withhold log-probabilities, so a rule defined over them fails on exactly the models most likely to be used; whether sampling recovers the same expected value is open. 

[Do not use 'moment' language ever]
And the second moment is untested: no study has asked whether the spread of the model's distribution tracks where human raters disagree, or is simply noise.



These gaps matter. 

A method validated on frequent concrete words can fail silently where it is needed most. 

[Segue .. ]
A recommendation pinned to log-probabilities cannot be followed on current frontier models. [wf does thiis mean" 'cannot be followed on current frontier models' -- is any one pinning recommnedations to log-probs -- not the standard approach -- ??]

[Segue .. ]
The spread is where much of the practical value sits: norming studies report SDs because disagreement is informative — it marks ambiguity, idiosyncrasy, and items that warrant caution — and a machine norm with a calibrated spread carries that information for free, while one without it returns point estimates that must be taken on faith. [This sentence is horrible -- long, em-dashes] 

[Make someone care here. tacked on]
A calibrated spread is also a harder test than a calibrated mean. A model can match means by reproducing population-level regularities; matching the structure of disagreement requires representing where the population itself is divided. 

And each gap bears on the same theoretical question [not clear here]: which psycholinguistic constructs are recoverable from language use alone, and which depend on perceptual and developmental experience that text only weakly encodes.



We benchmark four LLMs spanning size and provider — gpt-4o-mini, gpt-4.1, claude-haiku-4-5, and gpt-5.4-nano — against GloVe-300 and BGE-large-en-v1.5 ridge models on a stratified 1,638-word holdout spanning concreteness, frequency, and polysemy, across 18 dimensions, with lexical properties [which] as moderators in mixed-effects models. 



The benchmark is organized around three questions.

The first question is which approach [which appraoches ffs???] better approximates human norms, on which constructs, and with what word-level moderators. 

[STATE -- LLMs v Embeddings]
The two approaches use text differently. Elicitation draws on language use directly. Its alignment should therefore depend on how well a construct is stabilized in language use. Supervised regression instead learns a mapping from distributional features to human ratings, and can therefore exploit statistical correlates even of constructs that language encodes only weakly. 

If both points hold, neither approach should dominate, and we expect the relative advantage to vary by construct, with elicitation strongest on evaluative and experience-linked dimensions. 

At the word level, alignment should be best for words that are strongly represented (frequent), less reliant on context (low polysemy), and perceptually anchored (concrete).

The second question is whether a human norm is better approximated by the model's top token or by the mean of its response distribution. A norm is a mean over raters, so the distribution's mean is the like-for-like comparison, and we expect it to agree more closely. 

Temperature-1.0 outputs are draws from the same distribution, so averaging a small number of sampled ratings should recover the same expected value — a point with practical weight, since current frontier models withhold log-probabilities.

The third question is whether the distribution carries information beyond its mean. If it behaves like a simulated rater sample, its spread should covary with human inter-rater disagreement, most clearly on dimensions whose constructs are well recovered from language use.


[Bow here ... ?]

---





## Method

### Norm Dimensions

Eighteen psycholinguistic dimensions were included in the primary logprob analysis. [What?? In all analyses these are used -- and benchmark on integers would be primary anyway ..]

Eleven sensorimotor dimensions came from the Lancaster Sensorimotor Norms (Lynott et al., 2020): auditory, visual, haptic, interoceptive, gustatory, olfactory, foot/leg, hand/arm, head, mouth, and torso (scale 0–5). Three affective dimensions came from the Warriner et al. (2013) norms: valence, arousal, and dominance (scale 1–9). Four lexical dimensions were included: concreteness (Brysbaert et al., 2014; scale 1–5), socialness (Diveica et al., 2023; scale 1–7), iconicity (Winter et al., 2023; scale 1–7), and age of acquisition (Kuperman et al., 2012; scale 1–25).

[Why this set of norms??] 
[Should likely remove 'socialness' -- as it was crowd-courced 50% in India ffs -- remove, then we can say these are largely American English-based norms, eg]



### Vocabulary Sample

[We dont even need to get into lexis package here at all -- in fact complicates story]

[A total of 6,553 words occur across all 18 dimensions ... ]

[Introduce lexical moderators.]

Words were drawn from the `lexis` R package (Timm, 2025; https://github.com/jaytimm/lexis), an open resource compiled for this line of work that aligns multiple English psycholinguistic norm datasets with GloVe embeddings and lexical metadata in a single integrated table. 

The eligible pool was restricted to words with Lancaster ratings, concreteness ratings, age of acquisition ratings, valence-arousal-dominance ratings, iconicity ratings, socialness ratings, and a GloVe embedding, yielding approximately 6,553 words words. 

Socialness is the main coverage bottleneck in this overlap set, which is one reason earlier cross-dataset intersection designs tend to collapse onto relatively small vocabularies (cf. Kello et al., 2025).


A stratified sample of 6,553 words was drawn using a hybrid floor-plus-proportional allocation strategy across cells defined by concreteness quintile (5 levels) × Zipf frequency tertile (3 levels) × polysemy band (4 levels: 1 / 2–3 / 4–9 / 10+ synsets). 

Degree of polysemy for stratification purposes was operationalized from total synset count across parts of speech in WordNet (Miller, 1995), which provides a tractable coverage variable for ensuring the sample spans the polysemy range. 

Each occupied cell received a floor allocation of 40 words; remaining budget was distributed proportionally to cell capacity. The holdout set of 1,638 words (25%) was assigned within strata to preserve the stratified structure in both training and evaluation sets.


[this may be a diferent sub-section here -- ]

Two related lexical-ambiguity measures were distinguished. 

Polysemy proper was operationalized as WordNet synset count (Miller, 1995) — the variable used for stratification — entered into moderation on a log scale (log1p) to align the moderator with the sampling design. 

Grammatical versatility was operationalized as the number of distinct WordNet parts of speech in which a word has at least one synset (n_pos; range 1–4). 

These capture different sources of out-of-context ambiguity: synset count reflects fine-grained sense multiplicity, whereas n_pos reflects competition across grammatical roles (e.g., a word functioning as both noun and verb).


[This correlation is tenuous as it is hard to call a 1-4 pos count as a proper distribution.]
Because the two were only moderately correlated (Pearson *r* = .56) and showed low collinearity in the moderation model (variance inflation factors 1.5 and 1.8), both were entered jointly, so that each is estimated as a partial effect — synset multiplicity adjusting for POS spread, and vice versa. 


[Not sure this is relevant.]
Because the train/holdout split was stratified on synset bands rather than on n_pos, balance on grammatical versatility across the split is incidental rather than enforced; all reported analyses use the holdout only, so this does not affect the evaluation.




Table 1 summarizes the stratification design.

| Axis | Levels | Operationalization |
| --- | --- | --- |
| Concreteness | 5 | Quintiles from most abstract (Q1) to most concrete (Q5) |
| Frequency | 3 | Tertiles of Zipf frequency (low, mid, high) |
| Polysemy (stratification) | 4 | `1`, `2-3`, `4-9`, `10+` WordNet synsets |
| Polysemy (moderation) | continuous | log WordNet synset count (aligned with stratification) |
| Grammatical versatility | continuous | n_pos: distinct WordNet POS categories (1–4) |
| Sampling design | 60 cells | 5 × 3 × 4 hybrid floor-plus-proportional allocation |
| Sample split | 6,553 total | 4,915 training, 1,638 holdout, assigned within strata |




The resulting 6,553-word sample was closely balanced across concreteness quintiles (327–328 words per quintile) while preserving natural density differences across the frequency × polysemy cells, rather than forcing a uniform design.





Table 2 lists representative holdout strata illustrating the lexical range of the design.

| Stratum profile | Holdout *n* | Example words |
| --- | --- | --- |
| Abstract, low-frequency, high-polysemy (Q1 / low / 4–9) | 18 | `accessible`, `ascertain`, `assimilate`, `benevolent`, `consummate` |
| Mid-concreteness, mid-frequency, low-polysemy (Q3 / mid / 2–3) | 42 | `abduction`, `aphrodisiac`, `bankrupt`, `banter`, `bleep` |
| Concrete, high-frequency, monosemous (Q5 / high / 1) | 23 | `airport`, `blouse`, `cafeteria`, `cigarette`, `cocaine` |



The holdout set (*n* = 1,638) had the following lexical properties: concreteness *M* = 3.13, *SD* = 1.00 (range 1.2–5.0); Zipf frequency *M* = 3.49, *SD* = 0.67 (range 2.5–6.5); WordNet synset count *M* = 3.8, *SD* = 4.2 (range 1–75). The stratified design ensures representation across concreteness, frequency, and polysemy, including low-frequency abstract words that tend to be underrepresented in existing LLM norming evaluations.




### LLM Procedure

[the back and forth between integer and log-prob is fucking confusing a very simple distinction.]

LLM ratings were collected from four models spanning size and provider: gpt-4o-mini and gpt-4.1 (OpenAI), claude-haiku-4-5 (Anthropic), and gpt-5.4-nano (OpenAI). The integer benchmark uses all four models; gpt-4.1 is the strongest LLM in that benchmark and is featured when summarizing the representation comparison. 

The distributional analyses use the two OpenAI models that expose log-probabilities, and the sampling analysis uses a separate gpt-4o-mini run to test portability without logprob access. 

[For both log-prob and integer??]
Each word was presented as a single-turn prompt consisting of the dimension-specific rating instructions followed by the word on a new line, queried at temperature 0 with `max_tokens` = 1.


[no - we are not referencing lexis anymore -- instructions based on extant per task -- reworked here ....]
Instructions were drawn from the `norming_instructions` object in the lexis package, which provides standardized LLM-adapted versions of the original participant instructions for each norming dataset. 

These prompts were designed to balance fidelity to the instructions used in the source norming papers with straightforward, low-ambiguity wording suitable for one-shot LLM elicitation. The same standardized prompts were used for every model and were not re-tuned per model, so cross-model differences reflect the models rather than prompt engineering.

The top-token integer rating was recorded for all four models and drives the cross-model benchmark. 

[Horrible sentence]
Token log-probabilities — required for the expected-value and distributional-SD analyses — are exposed only by the two OpenAI models in this set (gpt-4o-mini, gpt-4.1); the GPT-5 series and Claude models do not provide them. 

Those analyses [which??] are therefore restricted to gpt-4o-mini and gpt-4.1, while the sampling procedure described below recovers the same expected value without log-probabilities and so applies to any model.



Table 3 gives representative prompt examples. These are not character-for-character reproductions of the original participant-facing instructions; they illustrate the prompt style used in the present study and the variation across construct types.

| Dimension | Example LLM prompt instruction |
| --- | --- |
| Concreteness | `Rate how concrete this word's meaning is. Concrete words refer to things experienced through the senses; abstract words refer to meanings defined through other words. Range: 1-5 (1 = abstract, 5 = concrete). Reply with one number.` |
| Socialness | `Rate how socially relevant this word's meaning is - does it refer to social characteristics, behaviors, roles, spaces, institutions, values, or interactions? Range: 1-7 (1 = not socially relevant, 7 = highly socially relevant). Reply with one number.` |
| Age of acquisition | `Estimate the age in years at which a typical English speaker first understands this word. Range: 1-25 (1 = learned very early, 25 = learned very late). Reply with one number.` |
| Valence | `Rate the emotional valence of this word. Range: 1-9 (1 = very unhappy/negative, 9 = very happy/positive). Reply with one number.` |
| Interoception | `Rate how strongly the meaning of this word is experienced through sensations inside the body (e.g., hunger, pain, heartbeat, fatigue). Range: 0-5 (0 = not at all, 5 = greatly). Reply with one number.` |
| Iconicity | `Rate how much this word sounds like what it means - an iconic word is one whose meaning you might guess without knowing English. Range: 1-7 (1 = not iconic at all, 7 = very iconic). Reply with one number.` |



**Integer ratings.** The top-sampled token was recorded and parsed as an integer.



**Logprob expected values.** For the two logprob-capable models (gpt-4o-mini, gpt-4.1), the same API call was made with `logprobs = TRUE` and `top_logprobs = 20`. For each word × dimension, all valid scale tokens appearing in the top-20 log-probability outputs were extracted, converted from log probability to probability, summed to compute coverage (the proportion of probability mass on valid tokens), and renormalized. The logprob-weighted expected value was then computed as the sum of (rating × probability) over valid tokens. Coverage was 1.000 across all 18 included dimensions. Although age of acquisition uses a 1–25 scale, the model's probability mass was concentrated within the recoverable token range, so effective coverage remained complete in practice.

For illustration, a concreteness prompt might return top scale-token log probabilities such as `"5"` = -0.11, `"4"` = -2.41, `"3"` = -4.10, with the remaining valid tokens carrying negligible mass. After exponentiation and renormalization over valid scale tokens, this corresponds approximately to *p*(5) = .89, *p*(4) = .09, *p*(3) = .02, yielding an integer rating of 5, an expected value of 4.87, and a distributional SD of 0.34. For a more ambiguous item, probability mass can be spread across several adjacent tokens; the expected value and SD then preserve information that the top token alone discards.


[Yr sampling integers here?? WE need to be consistent with language]
**Sampled expected values.** The logprob-weighted expected value requires an API that exposes token log-probabilities — an affordance that several current model families (the GPT-5 series, all Claude models) do not provide. To test whether the same quantity can be recovered without logprobs, we re-elicited every holdout word × dimension on gpt-4o-mini by sampling. Using the identical prompts, we drew *k* = 20 integer ratings per item at temperature 1.0 (issued as a single request with the OpenAI `n` parameter, so the prompt is processed once and 20 completions are returned), parsed each to an integer, and took their mean as a Monte-Carlo estimate of the model's expected rating.

Because the reported log-probabilities describe the model's native output distribution, sampling at temperature 1.0 draws from that same distribution; the sampled mean and the logprob-weighted expected value are therefore two estimators of one quantity. [This is not inutuitive]

All 20 draws were retained for each item (full per-draw distributions are archived with the run), and valid-token coverage was 1.000.



[Which procedure?? log-prob or sampled?] [And this should go with actual section describing the moethod no??]
This procedure follows recent work that uses internal token probabilities as graded evidence rather than relying on greedy decoding alone. The closest precedent in psycholinguistic norming is Martínez et al. (2025), who compared integer and probability-weighted ratings for multi-word expressions and found that the weighted version improved agreement with human norms; outside psycholinguistics, Argyle et al. (2023) treated token probabilities as continuous evidence. [No on this last citation -- there is a better one we have for locall LLMs -- although results arent great -- ]


[Perhaps relevant, but lacks context here -- make context]
The present study extends probability weighting to a broader set of constructs, adds a stratified lexical sample, benchmarks against supervised embedding models, and uses distributional spread as a second-order measure.



### Embedding Models

[Segue -- In order to ... ] [Ref 'Two computational routes now exist ....']
Two embedding models were evaluated: GloVe-300 (Carlson et al., 2025; 300-dimensional updated English GloVe vectors trained on Wikipedia, Gigaword, and a subset of Dolma) and BGE-large-en-v1.5 (BAAI, 2023; 1,024-dimensional sentence transformer trained with contrastive objectives on diverse text pairs). For BGE-large, single-word inputs were encoded with normalized embeddings. For each norm dimension, ridge regression (cv.glmnet, 5-fold cross-validation on the training set, alpha = 0) was fit on the 4,915 training words and evaluated on the 1,638 holdout words. [So, a total of 18 distinct models ...]


[Dont we want to lead with this -- then present model selected ... ] 
This follows the established distributional norm-prediction approach, in which lexical representations are mapped onto human norm scores by supervised models rather than elicited directly as judgments (Bestgen & Vincze, 2012; Mandera et al., 2017; Utsumi, 2020). 

The two were chosen to bracket the embedding spectrum: GloVe-300 is a classic static count-based vector model representing the long-standing distributional approach, whereas BGE-large-en-v1.5 is a strong contemporary contrastively trained sentence encoder. Spanning both guards against representing the embedding side by a single arbitrary model and shows the benchmark is not an artifact of one encoder's quality. Embedding model predictions and LLM ratings were evaluated on the same holdout set, enabling direct comparison.





### Statistical Approach

[This section is very difficult to follow -- has no inherent structure of any kind -- confounds all study sub-queries into one shite mess like previous section --]

All analyses used the holdout set only (*n* = 1,638 words). 

Methods were compared at the **dimension level** — each of the 18 dimensions contributes a single holdout Pearson *r*, rather than a word-level comparison of *r* within a dimension.
[Why?? is this standard across the literature?? citations??]



The expected-value-versus-integer improvement was tested with a paired *t*-test on Fisher-*z* transformed correlations across the 18 dimensions (with a supplementary sign test), while the per-dimension division of labor between LLM and embedding methods was characterized by bootstrapped 95% confidence intervals on each dimension's *r* rather than a single pooled across-dimension contrast.




Several secondary analyses followed. 

The cross-model benchmark compared integer ratings from the four LLMs against GloVe-300 and BGE-large ridge models, the latter fit via 5-fold CV on the 4,915 training words only (Friedman et al., 2010). [which latter?]

The expected-value comparison was pooled across the two logprob-capable models. 

We further replicated that expected value on gpt-4o-mini by sampling: the mean of 20 temperature-1.0 draws was correlated against both the logprob expected value and human norms across the full holdout, and a *k*-of-20 resampling analysis (200 resamples per *k*) characterized how agreement converges with the number of draws. 

The same sampled run was used to test recovery of distributional spread. 


Finally, we estimated method × lexical covariate interactions — concreteness, frequency, and the two polysemy measures (log synset count and grammatical versatility [n_pos]) interacted with method jointly — in a linear mixed-effects model (lme4; Bates et al., 2015) on range-normalized absolute prediction error (each absolute error divided by its dimension's response-scale span, so dimensions on different scales contribute comparably), with crossed random effects for word and dimension. [insane sentence -- parens, em-dashes, fucking nonsense]

This moderator model compared gpt-4.1 integer, the strongest LLM in the integer benchmark, against BGE-large; expected-value-specific lexical uniformity was tested separately within the two logprob-capable models.



To test whether the method differences are confounded with human-norm reliability, we estimated the reliability of each published mean norm as true-score variance divided by observed between-item variance, treating each item's measurement-error variance as SD² ⁄ *n* raters (for the Lancaster norms, which report no per-item rating count, *n* was taken as `N_known` for the corresponding perceptual or action modality set), and tested whether the per-dimension embedding advantage tracked it. [Single fucking esentence again -- ]



An exploratory [not exploratory] extension tested the association between logprob SD and human inter-rater SD, first on gpt-4o-mini and then pooled across both logprob-capable models with model added as a random intercept; published inter-rater SDs were an inclusion criterion for the study, so this analysis covered all 18 dimensions. Word prevalence frequency (Zipf scale) was retained as the frequency covariate across all analyses. Models were fit with REML; likelihood ratio tests used ML estimation.

---




## Results

[I like the 'representation' language here -- this is the first we see it -- should be used much earlier and more often]

[Per entire paper, no connection here between Part A and Part B --]

The Results are organized in two parts. Part A asks the representational question: when all methods are compared using the integer rating available across model families, which representation best predicts each human norm dimension? 

Part B then asks what is gained by reading the OpenAI models' response distribution rather than only their top token.



### Part A: Representation Benchmark

#### Multi-Model Benchmark

We first benchmarked four LLMs spanning size and provider — gpt-4o-mini, gpt-4.1, claude-haiku-4-5, and gpt-5.4-nano — against GloVe-300 and BGE-large. 


Because two of these model families (Claude and the GPT-5 series) do not expose token log-probabilities, this cross-model comparison uses the integer rating, the extraction rule available for every method. Expected-value refinements for the two OpenAI logprob models are reported in Part B. [This is fucking irrelevant here -- say in reslts intro -- no need to constantly repeat --]



**Descriptive correlations.** Mean holdout correlations (integer rating) were: BGE-large *r* = .734, GloVe-300 *r* = .652, gpt-4.1 *r* = .636, gpt-4o-mini *r* = .605, claude-haiku-4-5 *r* = .569, and gpt-5.4-nano *r* = .402 (Table 4). BGE-large had the highest *r* among the six methods on 13 of 18 dimensions. 

gpt-4.1, the strongest LLM overall, led on four dimensions (gustatory, olfactory, arousal, and age of acquisition), while gpt-4o-mini narrowly led on valence (.874 vs. .873). 

No other LLM and neither GloVe-300 nor BGE-large led on any of those five LLM-favorable dimensions. [wtf?? no]  



[We are not using Division of Labor language anymore -]
**The division of labor generalizes across model families.** The five dimensions on which an LLM leads are the same evaluative, hedonic, and lexical-experiential cluster across models — valence, arousal, gustatory, olfactory, and age of acquisition — whereas the embedding advantage concentrates on sensorimotor effector dimensions for every LLM. [this sentence is worded so confusingly -- ]  [its more akin to: all llms do best on same dims regardless of mod/size]


The models differ in level, not in shape: gpt-4.1 tracks BGE-large most closely, while gpt-5.4-nano collapses on abstract sensorimotor content (auditory *r* = .087, head .081, torso .160) yet still recovers valence (.808). This is the core division-of-labor result: the boundary follows the kind of lexical knowledge being predicted, not a single model idiosyncrasy.



**Table 4.** Holdout Pearson *r* with human norms by dimension and method (integer rating, 18 dimensions). The highest *r* among the six methods in each row is shown in bold. Reliability of each published mean norm is shown in the final column.

| Group | Dimension | gpt-4o-mini | gpt-4.1 | claude-haiku | gpt-5.4-nano | GloVe-300 | BGE-large | Reliability |
| --- | --- | --- | --- | --- | --- | --- | --- | --- |
| Lancaster | auditory | .393 | .337 | .348 | .087 | .679 | **.750** | .861 |
| Lancaster | visual | .578 | .611 | .552 | .318 | .599 | **.704** | .777 |
| Lancaster | haptic | .580 | .732 | .592 | .414 | .678 | **.734** | .869 |
| Lancaster | interoceptive | .650 | .637 | .633 | .579 | .726 | **.800** | .870 |
| Lancaster | gustatory | .829 | **.861** | .727 | .584 | .731 | .805 | .934 |
| Lancaster | olfactory | .751 | **.809** | .696 | .481 | .656 | .719 | .893 |
| Lancaster | foot/leg | .497 | .607 | .505 | .330 | .564 | **.686** | .834 |
| Lancaster | hand/arm | .471 | .664 | .512 | .249 | .590 | **.702** | .816 |
| Lancaster | head | .363 | .257 | .219 | .081 | .534 | **.628** | .618 |
| Lancaster | mouth | .613 | .594 | .474 | .232 | .630 | **.728** | .843 |
| Lancaster | torso | .381 | .523 | .363 | .160 | .590 | **.709** | .792 |
| VAD | valence | **.874** | .873 | .866 | .808 | .761 | .842 | .927 |
| VAD | arousal | .661 | **.703** | .668 | .609 | .554 | .650 | .693 |
| VAD | dominance | .556 | .250 | .243 | .284 | .668 | **.742** | .788 |
| Lexical | socialness | .769 | .776 | .751 | .567 | .771 | **.844** | .894 |
| Lexical | age of acquisition | .812 | **.852** | .825 | .588 | .733 | .737 | .922 |
| Lexical | concreteness | .811 | .871 | .774 | .681 | .814 | **.879** | .947 |
| Lexical | iconicity | .295 | .490 | .485 | .191 | .456 | **.546** | .538 |
| **Mean** | | **.605** | **.636** | **.569** | **.402** | **.652** | **.734** | **.823** |

*Note.* All LLM columns are the top-token integer rating (the extraction rule common to all model families); expected-value correlations for the two logprob models (gpt-4o-mini, gpt-4.1) are reported in the *Distribution Mean* section. gpt-4o-mini has the highest valence correlation (.874), narrowly above gpt-4.1 (.873). Group labels (Lancaster, VAD, Lexical) organize the rows by norm source and are descriptive only; socialness is grouped with the lexical dimensions following the Method (Diveica et al., 2023). No analysis treats these groups as a factor. *Reliability* is the reliability of each published mean norm, estimated as true-score variance ÷ observed between-item variance from the shipped per-item SD and rater count (measurement-error variance = SD² ⁄ *n* raters); for the Lancaster norms, which report no per-item rating count, *n* is `N_known` for the perceptual or action modality set. [Again, stfu about log-probs here] [Reliability should be defined prior to table -- it is confusing enough]



**Mixed-effects model.** The accuracy ordering was confirmed in a linear mixed-effects model on range-normalized absolute error (each word × dimension absolute error divided by its dimension's response-scale span), with method as a fixed factor and crossed random effects for word and dimension. With BGE-large as reference, every LLM had reliably higher error (GloVe-300 β = +0.059; claude-haiku +0.420; gpt-4o-mini +0.434; gpt-4.1 +0.554; gpt-5.4-nano +0.871; all *p* < .001). [Which means??]

The integer benchmark therefore has a clear overall winner, but the per-dimension pattern shows that the overall ranking is not the scientific story.[unclear why not?? explain--] 




#### Lexical Moderators

The introduction set a word-level expectation alongside the construct-level one: alignment should be best for words that are strongly represented (frequent), less reliant on context (low polysemy), and perceptually anchored (concrete). The moderation analysis asks whether those gradients shape difficulty for every method and whether they moderate the gap between methods. If the method differences were driven by the covariates rather than by the representations, they would not generalize beyond the sampled lexicon.

We addressed this by extending the mixed-effects model to interact method with all four lexical covariates jointly (concreteness, frequency, log synset count, and grammatical versatility [n_pos]), so that each interaction is estimated controlling for the others. This analysis compared gpt-4.1 integer, the strongest LLM in the integer benchmark, with BGE-large. An omnibus likelihood-ratio test (ML) confirmed that the interaction block as a whole improved fit, χ²(4) = 513.7, *p* < .001. Concreteness was the dominant moderator: added last, over a model already containing the frequency, synset, and n_pos interactions, the method × concreteness interaction contributed χ²(1) = 413.2, *p* < .001.


**Concreteness.** Main effect β = −0.014 (*t* = −18.4). BGE × concreteness β = +0.018, 95% CI [+0.017, +0.020] (*t* = 20.4, *p* < .001). The embedding advantage over the strongest LLM integer rating was largest for abstract words. This is the most robust lexical moderator: it is much larger than the other interaction terms and is stable across error metrics (raw, range-normalized, and within-dimension *z*-scored).


**Frequency.** The frequency interaction was reliable but small: BGE × frequency β = −0.006 (*t* = −6.0). It is roughly a third the magnitude of the concreteness interaction and opposite in sign; we treat frequency as a secondary moderator rather than a co-equal of concreteness. [direction of effect?]


**Polysemy and grammatical versatility.** In the gpt-4.1 integer versus BGE comparison, both secondary ambiguity interactions were reliable but smaller than concreteness: BGE × log synset count β = +0.007 (*t* = 6.1), and BGE × n_pos β = +0.004 (*t* = 3.5). These effects indicate modest shifts in the embedding advantage across ambiguity structure, but they are not the main driver of the benchmark. [direction of effects?]

The dominant lexical result remains the abstract-word embedding advantage indexed by concreteness.

Method differences [then?] were not reducible to these covariates. After residualizing predictions on concreteness, frequency, log synset count, and n_pos within each dimension, ordering was preserved: gpt-4.1 integer *r*_resid = .572, BGE-large .639. The benchmark is therefore not driven by a favorable lexical slice; the method hierarchy persists after controlling the main lexical gradients built into the sample design.





#### Norm Reliability Is Not the Source of the Reversal

Before the per-dimension pattern can stand as the answer to the first question [wtf is this wind-up?? cmon], one alternative explanation needs ruling out: differential reliability of the human norms. [Maybe this should go after you attempt to explain per-dimension first ffs, thenre but -- jfc]

A noisier published mean caps the achievable correlation for every method, so the LLM might appear to lose on sensorimotor dimensions simply because those norms are less reliable than affective ones. Reliability here is the repeatability of a published *mean* norm: the share of between-word variance in the norm that remains after accounting for rater noise, estimated from each item's reported SD and number of raters. It is distinct from the raw inter-rater SD analyzed in Part B. Reliability uses SD²/*n* as measurement error in the mean; inter-rater SD is treated later as a possible semantic signal in its own right.

The reliability analysis rules out the artifact explanation [what artifact?]. Reliability bounds achievable accuracy for both methods: it correlates positively with holdout *r* for gpt-4.1 integer (*r* = .64) and BGE-large (*r* = .88). But because it constrains both methods, it does not by itself produce the *gap* between them. Across the 18 dimensions the embedding advantage (BGE − gpt-4.1 integer) was not positively related to reliability (*r* = −.33, *p* = .185), the opposite direction from what the artifact account predicts.

The decisive evidence is dimensional. Large embedding advantages still fall on highly reliable sensorimotor norms: auditory (reliability .861, gpt-4.1 integer *r* = .337 vs. BGE .750), interoceptive (.870, .637 vs. .800), and mouth (.843, .594 vs. .728). Reliability is also high where the LLM is competitive or ahead: gustatory (.934), olfactory (.893), and age of acquisition (.922). A reliability ceiling that is high on both sides of the split cannot explain why the LLM tracks taste, smell, and AoA but not auditory or interoceptive strength.

The reversal is therefore a property of what each representation recovers, not of how cleanly the norms were measured. Attenuation-corrected correlations preserve the same ordering and the same per-dimension pattern.

[I still dont understand any of this]



#### Predicted-Range Calibration

[This is completely out of the fucking blu -- bo context -- no segue -- uncleaar why it is even being discussed -- do better -- ]

[this does not read like a results section at all --] [need table] [the language surrondingg this line of inquiry has been insocnsistent throughout paper]

[Does this belong in Part B??]

A practical complement to the first question [which is that?] concerns the scale of predictions rather than their ranking. High rank correlation does not imply a calibrated spread, and the two approaches miscalibrate in opposite directions. 

Because ridge regression estimates a conditional mean, its fitted values are attenuated relative to the truth by approximately the validation correlation: the embedding methods compressed the dynamic range of the norms they produced, with prediction SDs only .72 (BGE-large) and .63 (GloVe-300) of the human SD.

Direct LLM elicitation is not a least-squares estimator and carries no such structural attenuation. In the gpt-4.1 integer analysis, the LLM prediction SD averaged 1.35× the human SD (1.73× on affective dimensions, 1.42× on lexical dimensions, and about 1.22–1.23× on sensory and effector dimensions). 

The distinction is invisible in *r* but matters once generated norms are used rather than merely validated: compressed embedding norms pull extreme items toward the mean and bias downstream effect sizes toward zero, whereas elicited norms preserve or inflate the spread. 

For ranking, correlation is the right yardstick; for building a norm set whose absolute values and spread will be used, embedding output needs variance correction or calibration.



#### Per-Dimension Division of Labor

[segue -- as we have presented these findings inititally in table -- then went on to talk about reliability -- flow issues all around here]

The interpretive weight rests on the per-dimension pattern rather than on any coarse grouping of dimensions. In the integer benchmark, an LLM led on five dimensions: valence (gpt-4o-mini *r* = .874), gustatory (gpt-4.1 *r* = .861), age of acquisition (gpt-4.1 *r* = .852), olfactory (gpt-4.1 *r* = .809), and arousal (gpt-4.1 *r* = .703). For the featured Part A comparison, each gpt-4.1-vs-BGE difference was assessed with a paired bootstrap (2,000 resamples, fixed seed), scored on the same resampled words.

In the gpt-4.1 integer comparison, the paired interval excluded zero in the LLM's favor for age of acquisition (BGE − LLM Δ = −.115 [−.138, −.094]), olfactory (−.090 [−.127, −.054]), gustatory (−.056 [−.092, −.024]), arousal (−.054 [−.078, −.031]), and valence (−.031 [−.046, −.016]). Haptic was effectively tied (.002 [−.028, .032]). In the other direction, BGE-large held reliable advantages on the effector dimensions and on dominance, interoception, auditory, mouth, torso, socialness, iconicity, foot/leg, hand/arm, visual, and head.


[Good -- reuse]
The verdict is graded rather than categorical: a few reliable LLM wins concentrated on the most verbalized evaluative and lexical-experiential dimensions, set against a broad and consistent embedding advantage across sensorimotor space.





### Part B: The Model's Response Distribution

Part A used the integer rating because it is the extraction rule common to all model families. But a published human norm is itself a distribution over raters, summarized by its mean and SD, and a single LLM query likewise yields a distribution over scale tokens with a mean and SD of its own. [Already stated -- this is a results setion -- share results]

Part B takes the second and third questions in turn: whether the human norm is better approximated by the distribution's mean than by its top token, and whether the distribution's spread carries information about human disagreement. For both, it also asks whether the same quantities can be recovered without direct logprob access.



#### Distribution Mean: Expected Value vs. Integer Rating

The second question's expectation was that the expected value would track human norms more closely than the top token, both being means. It did. [No -- no rhetorical short sentece shite like this EVER!!]

For gpt-4.1, the strongest LLM in the integer benchmark and one of the two models with log-probability access, the logprob-weighted expected value outperformed the integer rating on all 18 dimensions (mean *r* = .646 vs. .636; Δ*r* = .010; *t*(17) = 10.4, *p* < .001). 

The gain was smaller than for gpt-4o-mini, where the expected value also beat the integer rating on all 18 dimensions (mean *r* = .628 vs. .605; Δ*r* = .023; *t*(17) = 9.99, *p* < .001), [new sentence with init transition] consistent with the stronger model's top token already being more dominant. 

Pooling both logprob models in a single mixed model confirmed the extraction effect: the expected value lowered absolute error relative to the integer rating, β = −0.026, 95% CI [−0.035, −0.018], *t* = −6.0, *p* < .001.



The expected-value improvement was lexically uniform. In a separate extraction × lexical-covariate model over the two logprob-capable models, EV × concreteness was β = −0.0004, 95% CI [−0.0019, 0.0011], and EV × frequency was β = −0.0001, 95% CI [−0.0018, 0.0015]. The EV gain is therefore a measurement refinement inside the LLM pipeline rather than a new representation that selectively fixes abstract or low-frequency words. 

This matters for interpreting the benchmark: crediting the OpenAI models with their expected values improves their scores, but it does not erase the broader embedding advantage.



The resolution logic explains why the gain is small but reliable. Human aggregate norms are continuous means; they occupy sub-integer space. A top-token integer is locked to integer resolution by construction, so it cannot capture the part of the target that lies between adjacent scale values. The expected value recovers that sub-integer information without changing the underlying judgment. 

[Explain this better -- and we need to bring 'rounded human norm' piece into fold more -- this is throw away -]
In the gpt-4o-mini resolution check, the human norm correlated .957 with its own rounded value, and rounding the LLM expected value back to the nearest integer erased most of the EV-over-integer gain. The expected value is therefore not a claim that decimals are intrinsically better; it is the structurally matched object for a continuous human mean.


[Lots of reference to 'resolution check' -- wtf is the resolution check??]




#### Distribution Spread: Logprob SD and Human Inter-Rater Disagreement

[for entire sections here, not clear wtf model we are talking about?]

The third question asked whether the distribution's spread carries information beyond its mean, with the expectation that model spread would covary with human inter-rater SD most clearly on dimensions whose constructs are well recovered from language use. 

Logprob SD varied substantially across dimensions (dimension means ranging from *M* = 0.10 for gustatory to *M* = 1.49 for age of acquisition, the latter reflecting its wider 1–25 scale); both logprob SD and human inter-rater SD were standardized within dimension before analysis. 

On gpt-4o-mini, a mixed-effects model (human inter-rater SD as outcome, logprob SD as predictor, with a random slope by dimension) showed a reliable positive main effect of logprob SD (β = 0.132, *p* = .002). 


The effect strengthened when pooled across both log-probability-capable models, with model as an additional random intercept: β = 0.181, 95% CI [0.119, 0.243], *t* = 5.7, *p* < .001.

The pattern is selective, as the third question anticipated. Dimension-level correlations (z) were positive for semantically rich dimensions (concreteness *r* = .413, age of acquisition .401, gustatory .398, olfactory .247, visual .222) and near zero or negative for floor-heavy Lancaster body-part norms (hand/arm *r* = .020, torso −.090, auditory −.097). 

Logprob spread therefore tracks human disagreement where disagreement plausibly reflects semantic indeterminacy, and loses calibration where the human scale is compressed near floor or the disagreement reflects embodied and individual differences not encoded in text.




#### Recovering the Distribution by Sampling

The expected-value extraction above presupposes access to token log-probabilities. That access is not guaranteed: several current model families — the GPT-5 series and all Claude models — withhold logprobs. [Yes stated multiple times -- this is a results section again -- ]

We therefore asked whether the same distributional quantities can be recovered by sampling alone. Using the identical prompts, we re-elicited every holdout word × dimension on gpt-4o-mini by drawing *k* = 20 integer ratings at temperature 1.0 and taking their mean as a Monte-Carlo estimate of the model's expected rating.

Across the full holdout (29,484 word × dimension cells), the sampled expected value was nearly identical to the logprob-weighted expected value from the same model, *r* = .997 (per dimension, *r* ranged from .970 for iconicity to .998 for valence). The two estimators also agreed with human norms to the same degree: the sampled expected value correlated with human norms at *r* = .902, against *r* = .901 for the logprob expected value. Sampling, then, reproduces the distributional extraction at no cost to human agreement.



**Table 5.** Agreement of the sampled expected value with the logprob expected value and with human norms, as a function of the number of draws *k* (full holdout; 200 random *k*-of-20 resamples).

| Draws (*k*) | *r* with logprob expected | *r* with human norms |
| --- | --- | --- |
| 1  | .977 | .884 |
| 2  | .988 | .893 |
| 3  | .991 | .897 |
| 5  | .994 | .899 |
| 10 | .996 | .901 |
| 15 | .997 | .902 |
| 20 | .997 | .902 |

*Note.* Reference ceiling: logprob expected value vs. human norms *r* = .901. Token coverage was 1.000; occasional draws that failed integer parsing were dropped, with the item mean computed over the remaining draws.




The same logic extends to the distribution's spread. The SD of the 20 sampled draws recovered the logprob SD strongly overall, *r* = .927 across the 18 dimensions, with per-dimension correlations ranging from .64 for iconicity to .91 for gustatory. Thus the sampling route does not merely approximate the distribution's mean; it recovers its spread as well. 

For models that expose log-probabilities, mean and spread are available analytically from a single call; for models that do not, both can be recovered by averaging and summarizing a modest number of sampled ratings.

Part B's answers to the second and third questions are therefore parallel: the distribution's mean aligns with the human norm, and its spread aligns selectively with human disagreement. Mean and spread carry signal on the same text-accessible dimensions, and both are recoverable by either logprob access or sampling. 

That is the distributional measurement claim; the representation claim remains the Part A reversal.  [stfu with this -- ]




## Discussion

The three questions posed in the introduction receive one answer in three forms. Which representation better approximates human norms depends on the construct: embedding regression is stronger wherever prediction can draw on implicit distributional structure, elicitation wherever the relevant meaning circulates in text as evaluation or shared experience. The distribution mean outperforms the top token everywhere, because a human norm is a mean over raters and the expected value is its structural analogue. And the distribution spread tracks human disagreement selectively, on the dimensions whose constructs are well recovered in the first place. The three results sit on one boundary, between lexical knowledge that is recoverable from language use and lexical knowledge that raters supply from embodied and developmental experience. The sections below take the questions in order.

### Which Representation, for Which Constructs

BGE-large had the highest *r* among the six methods on 13 of 18 dimensions, with mean holdout *r* .098 above the strongest LLM integer rating (.734 vs. .636 for gpt-4.1). Crediting gpt-4.1 with its expected value narrows the gap but does not change the pattern: the embedding advantage is broadest on the Lancaster sensorimotor dimensions, and the LLM advantage concentrates on the evaluative and experience-linked cluster. The pattern is consistent with prior LLM norming work, which repeatedly finds stronger performance on lexical and affective judgments than on embodied effector dimensions (Trott, 2024a; Hagihara & Miyazawa, 2026; Conde, González, et al., 2025; Xu et al., 2025). Xu et al. (2025) report the same split for GPT-4 on the Glasgow and Lancaster norms, treating the model as a single aggregated participant; the present design adds the embedding competitor, the stratified lexicon, and the distributional reading that turns that single participant into a rater sample.

The reversal is not a model idiosyncrasy. Across all four families the same cluster favors elicitation and the same sensorimotor dimensions favor embeddings; the models differ in level, not in shape. gpt-5.4-nano collapses on abstract sensorimotor content (auditory *r* = .087) yet still recovers valence (.808), and gpt-4.1 tracks BGE-large most closely without overturning it. Increasing capability raises agreement across the board, most where the construct is already partly encoded in evaluative and experiential discourse; it does not manufacture embodied signal that text does not carry. The boundary holds from the weakest model to the strongest, which is what identifies it as a property of the two routes rather than of any vendor's training run.

The two-source view of rating introduced at the outset says why the boundary falls where it does. A human rater draws on distributional experience and on embodied experience (Barsalou, 2008); both machine routes have only the first. They use it differently, however. Supervised regression needs the relevant structure merely to be somewhere in the vector, accumulated implicitly across contexts, whereas zero-shot elicitation must convert a word into an explicit scalar judgment. Properties that speakers routinely verbalize, such as pleasantness, disgust, intensity, and when a word is learned, survive that conversion. Properties that speakers enact but rarely describe, such as which effector a word involves, do not; their traces in text are diffuse, and diffuse traces are exactly what a supervised map over a dense vector can exploit and a prompted judgment cannot. On this reading the embedding advantage is not evidence that embeddings are grounded; it reflects the difference between regression over continuous representations and introspective elicitation, applied to constructs whose public verbalization varies. Some Lancaster dimensions may also be awkward rating targets rather than merely difficult ones: judgments of how strongly a word relates to `foot/leg`, `torso`, or `hand/arm` compress an unusual metalinguistic decision into a single scalar response, and for human participants and LLMs alike these are less natural introspective tasks than judging valence or concreteness.

The moderator analysis locates the same boundary inside the lexicon. The embedding advantage concentrates on abstract words (method × concreteness β = +0.018, much the largest lexical interaction); abstract words are where explicit evaluative description thins and meaning is carried across many contexts, and they are the region that convenience samples of concrete, frequent words never reach, which is why the moderation has gone unobserved. Frequency shows a much smaller interaction, roughly a third the size and opposite in sign, and we do not lean on it. Grammatical versatility (n_pos) also predicts norming difficulty, but it does not selectively moderate the embedding advantage: words appearing across multiple parts of speech are harder to norm out of context for all methods, consistent with the sense-competition account (Reijnierse et al., 2019). When the two polysemy measures were entered together, this versatility effect held while fine-grained synset count was inert on difficulty, so the relevant ambiguity is competition across grammatical roles rather than sense multiplicity per se.

Utsumi's (2020) domain hierarchy makes the same point from the embedding side: distributional vectors carry graded but uneven recoverability across semantic domains, with abstract and social-cognitive features predicted best and heavily grounded domains less well, though not at zero. That hierarchy rests on 535 words evaluated by leave-one-cluster-out cross-validation rather than an independent holdout, so it is suggestive rather than definitive; the present results extend the argument to a stratified 1,638-word holdout in which embedding regression is directly compared with prompted LLM judgments.

### Why the LLM-Favorable Cluster Is Evaluative and Experience-Linked

An LLM led on valence, gustatory, age of acquisition, olfactory, and arousal. In the cross-model integer benchmark, gpt-4.1 led on four of these and gpt-4o-mini narrowly led on valence; under expected-value extraction, the same cluster remains the LLM-favorable set. Four are affective or hedonic; age of acquisition extends the LLM advantage to an experience-linked lexical property with a strong distributional-history signature. These dimensions are not generically easy. They are unusually available in language as evaluative or experience-linked summaries: words are routinely used in contexts that communicate positivity, intensity, pleasantness, unpleasantness, typical acquisition history, or shared impressions of smell and taste. A prompted LLM can use that signal directly.

The norm dimensions also differ in their score distributions. Affective and lexical variables are broadly graded across the lexicon, whereas several sensory and effector dimensions are strongly floor-distributed. Gustatory (83% of holdout words at floor) and olfactory (76% at floor) are the most extreme cases: the task is closer to sparse detection, identifying which words belong in a small positive tail, than to graded estimation. Most Lancaster effector dimensions are intermediate. Model performance is therefore not evaluated against identical target structures across dimensions, and method advantages partly reflect how well each approach handles different prediction regimes. Distributional shape alone does not determine the winner, however: although the LLM leads on the floor-heavy gustatory and olfactory dimensions, supervised embeddings remain stronger on other floor-leaning bodily dimensions such as haptic, foot/leg, and torso. Sparseness is one contributor to the dimension-specific pattern, not its sole cause.

This helps explain why gustatory and olfactory behave differently from the other Lancaster dimensions. As perceptual systems, taste and smell are not well represented in direct linguistic description, but as evaluative channels they are pervasive: in ordinary text, and especially in review genres centered on food, drink, perfume, and consumer experience, these meanings are expressed through pleasantness, disgust, flavor, freshness, rancidity, sweetness, and other hedonic cues. LLMs appear to benefit from this evaluative description, whereas embedding models remain stronger when the task depends on implicit bodily involvement such as torso or hand/arm engagement. Kurfali et al. (2025) support the broader claim for olfaction: language models perform poorly on non-verbal perceptual odor similarity but substantially better on linguistically mediated odor judgments, where evaluative and descriptor-based information is available in text.

Age of acquisition broadens the interpretation of the LLM advantage. It is not an affect rating, yet it is tied to distributional history, educational context, and shared intuitions about when words are learned. Clark and Paivio (2004) supply a mechanism: in their factor analysis, age of acquisition loaded across familiarity, length, concreteness, and frequency, and they suggest that raters judging acquisition age are in effect judging how familiar, short, and concrete a word is. If AoA ratings are composite judgments over such text-accessible cues rather than autobiographical memory of learning, their presence in the LLM-favorable cluster is predicted rather than anomalous. That the LLM expected value leads here suggests that zero-shot elicitation can be strongest not only for affective content but also for experience-linked lexical properties that are culturally and linguistically overlearned.

### The Mean of the Distribution

A published norm is a mean over raters; a top token is the model's single most probable answer, the verdict of one simulated speaker. Reading the expected value is the move from that speaker to the community-level quantity a norm actually is, and it paid consistently: the expected value outperformed the integer rating on all 18 dimensions for both log-probability-capable models, with the larger gain on the weaker model (Δ*r* = .023 for gpt-4o-mini, .010 for gpt-4.1) and a pooled error reduction of β = −0.026. The improvement is a resolution correction. Human aggregate norms occupy sub-integer space; a top token is locked to integer resolution by construction; the expected value recovers the part of the target that lies between adjacent scale values without changing the underlying judgment, which is why rounding it back to the nearest integer erased most of the gain. The gain was also lexically uniform, with no reliable interaction with concreteness or frequency, so the expected value is a measurement refinement inside the elicitation pipeline rather than a new representation that selectively repairs abstract or rare words. The Part A and Part B results are in that sense complementary: reading the distribution improves the LLM estimate everywhere, and embeddings remain the better predictors over much of the lexical space.

The improvement was small, and that is informative. With the extraction rule fixed, the remaining gaps between methods are about representation rather than scoring; no better reading of the distribution turns an elicited torso judgment into an embedding-grade prediction. The distributional reading also bears on crowd-equivalence estimates: Trott's (2024b) number-needed-to-beat values are computed from temperature-0 top tokens, so under a distributional reading they are floors, and the effective crowd a model reflects grows when its distribution rather than its mode is scored. Because sampling at temperature 1.0 recovers the same expected value for model families that withhold log-probabilities, none of this is confined to one vendor's API.

### The Spread of the Distribution

A published norm also reports an SD over raters, and Part B's parallel finding, that logprob SD tracks human inter-rater SD (pooled β = 0.181, *p* < .001), enters a debate about what that variability means. The early norming tradition treated inter-rater SD as a reliability diagnostic, a screen for unstable items to be excluded before experimental use. That consensus has since fractured.

Brainerd and colleagues reframed valence SD as *valence ambiguity*, a psychological property that predicts memory and recognition independently of mean intensity, and later extended the logic across 21 semantic attributes (Brainerd, 2018). Pollock (2018) showed that many abstract words have SDs near the theoretical maximum because raters split between sense-dependent interpretations, which indexes lexical non-determinacy rather than measurement failure. For age of acquisition, work on German norms established that means and SDs are positively correlated: later-acquired words attract more disagreement in structurally meaningful ways (Birchenough et al., 2017). Muraki et al. (2025) extend the argument to sensorimotor norms: raters with more vivid motor imagery give graspability ratings further from the mean, so part of the rating variance is systematically related to rater characteristics rather than random. The semantic diversity tradition (Hoffman et al., 2013) treats variability in contextual word usage as a property of the word rather than the rater.

Logprob SD is distinct from both. It is not rater SD: it carries none of the individual-difference confounds that make human SD hard to interpret (response style, scale heteroscedasticity, individual embodiment). It is also not semantic diversity in Hoffman's sense, because it is elicited through a prompted rating task rather than computed from corpus usage. It sits between the two — the model's posterior over scale values, derived from training on text that reflects how a word is used and discussed across many speakers and contexts.

The tracking is selective. Logprob SD correlates most strongly with human inter-rater SD on concreteness (*r* = .413), age of acquisition (.401), and gustatory (.398) — the dimensions where the norming literature has independently established that SD is signal rather than noise. Concreteness SD reflects sense-dependent ambiguity (Pollock, 2018); AoA SD reflects acquisition-history variability; gustatory and olfactory SD reflect evaluative-hedonic indeterminacy for words in domains with rich descriptor-based discourse. On floor-heavy Lancaster effector dimensions the association is near zero or negative (torso *r* = −.090, auditory −.097); these cases are artifacts of scale compression, because when most words score near zero, human SD is bounded near floor and logprob SD has no signal to track. Muraki et al. (2025) add that some sensorimotor rating variance relates to raters' imagery vividness rather than to word-level non-determinacy. A text-trained model has no idiolect, and logprob SD cannot track variance grounded in individual motor imagery rather than in linguistically encoded semantic structure.

These results bear on a question Trott (2024b) raises but does not pursue: what a norm contains when individual speakers do not converge on a judgment. The aggregate norm score is an idealization — the central tendency across speakers who bring different experiential histories, contexts, and acquisition ages to a word, and which no single speaker instantiates. The distinction is the classic one between the speech community and the speaker: ratings are sourced in individual linguistic experience, but the norm aggregates over a community that no individual member embodies (Timm, 2016). On this view the mean and the SD encode different things: the mean is that idealized average, and the SD is the structure of individual variation the average conceals (Hoffman et al., 2013; Reijnierse et al., 2019; Muraki et al., 2025). The present finding is that a text-trained model's probability distribution partially recovers that concealed structure — selectively, on the dimensions where the variation is semantically meaningful rather than floor-driven — without any representation of individual speakers.

The spread result also answers a question that same work poses as open. Trott (2024b) asks whether temperature or prompting can induce stochasticity in LLM responses that resembles human variance, or whether the models are simply ill-suited to individual differences; Li, Li, and Qiu (2025) answer pessimistically for opinion surveys, where silicon samples systematically under-disperse relative to human respondents. The present results return a conditional answer: the model's spread is calibrated to human disagreement on semantically rich dimensions and homogenized elsewhere, and the condition is the same construct boundary that organizes the benchmark.

The second channel is not independent of the benchmark: the dimensions where the LLM's mean best matches human norms are the same ones where its logprob SD best matches human disagreement. The mean and the spread of the distribution thus carry the same representational signature — a text-trained model recovers not only the expected rating but something of its dispersion, on the dimensions where both are recoverable from linguistic evidence.

### Implications

These results carry three practical implications for norm extension. The first is that the choice between elicitation and embedding regression should follow the construct. For affective, hedonic, and some experience-linked dimensions such as age of acquisition, LLM expected values are competitive with or better than embedding models; for sensorimotor dimensions, and likely for other constructs that require inferred bodily involvement, supervised embeddings remain preferable. One caveat tempers the embedding recommendation: because ridge regression estimates a conditional mean, embedding-predicted norms are range-compressed (predicted SD ≈ .72 of the human SD for BGE-large), so when a norm set's absolute values and spread will be used downstream rather than only its rankings, the embedding output needs variance correction, whereas direct elicitation does not attenuate.

The second is that an LLM's rating distribution should be read in full. For models that expose log-probabilities, the expected value is available analytically from the same call, adds no cost, and provides a coverage diagnostic that enables quality control; for models that do not, including the GPT-5 series and Claude models, the same value is recoverable by averaging five to ten temperature-1.0 samples (*r* = .994–.996 against the logprob expected value, .997 by 20 draws). Coverage was complete in the present study; for narrow scales or models with different tokenization it could fail, which makes it a useful flag.

The third is that the lexical composition of the norming sample is itself a scientific variable. Samples biased toward high-frequency, concrete, monosemous words will overestimate LLM accuracy relative to the full vocabulary and underestimate the embedding advantage, and the moderator analyses reported here would be invisible in them. Stratified sampling across concreteness, frequency, and polysemy is therefore a precondition for generalizable conclusions.

### Limitations

The benchmark spans four LLMs across two providers, which substantially addresses the single-model concern: the construct-dependent reversal and the integer-tier ordering replicate across families (above). Two residual scope limits remain. First, the expected-value and logprob-SD analyses rest on only two log-probability-capable models (gpt-4o-mini, gpt-4.1), both from one provider; although the sampling equivalence (*r* = .997) shows the same quantity is recoverable elsewhere, the analytic expected value itself was validated only within the OpenAI family.

A broader limitation is that LLM papers study moving and sometimes sunsetting targets. We address this by designing the claims around invariants rather than leaderboard numbers. The specific correlations reported here have a short half-life; the structural claims should not. A human norm is an expected value, sampling is a Monte-Carlo route to the same distributional quantity, and the benchmark asks which kinds of lexical knowledge are recoverable from text-based elicitation versus supervised distributional representations. The capability gradient across models is therefore a space-for-time check on the boundary, while the pinned model IDs, raw responses, and archived runs freeze the empirical specimens so the reported numbers remain auditable after endpoints change.

Second, the prompt instructions were standardized within the lexis package and were not re-tuned per model, so cross-model differences reflect the models under a common prompt rather than each model's best-case elicitation; different instruction formulations could shift performance, particularly on ambiguous or awkwardly phrased dimensions such as the Lancaster body-part scales, where judging how much a word relates to `foot/leg` or `torso` is not an especially natural rating task for human participants or LLMs, even if the resulting norms are still useful. Temperature-1.0 sampling was used here for the equivalence test and did not degrade agreement with human norms; we did not otherwise vary chain-of-thought or few-shot prompting, which could further improve LLM calibration.

The human norms are themselves generation-stamped. Ratings are sourced in the linguistic experience of particular cohorts — Warriner et al.'s raters worked around 2012, Lancaster's around 2019 — whereas the models are trained on text with a later center of mass. Some residual misalignment is therefore expected even on well-recovered dimensions, concentrated on words whose usage has moved. The one available human anchor suggests the ceiling: imagery and familiarity ratings collected three decades apart correlate at .80 to .85 (Clark & Paivio, 2004), so exact agreement with a fixed norm is not the right target for a rater whose linguistic experience postdates the norm's.

The rater populations are also heterogeneous across the 18 source norms. Most were collected from North American crowdworkers, but the socialness norms were collected on Prolific from a sample in which roughly half the raters reported English as a second language (Diveica et al., 2023). Population mismatch between raters and training corpora, in dialect, era, and language background, is a residual confound the design does not isolate, and one that varies by dimension.

Age of acquisition warrants scrutiny because the scale runs from 1 to 25 while `top_logprobs` returns only 20 tokens. In practice, mean coverage was .999 and the full 1–25 rating range appeared in the recovered distributions, with 36% of items having coverage below 1.000 (minimum .984). The method performs adequately for AoA in this dataset, but coverage should be verified when applying it to other scales or models. Published split-half reliability estimates were not available in a comparable form across all 18 source norms, so we estimated each norm's reliability from its shipped per-item SD and rater count (Table 4); this is an approximation that assumes the per-item SD reflects rater disagreement rather than, for example, scale-floor compression, which may understate reliability on the most floor-heavy dimensions. The reliability-corrected correlations reported in the Results should therefore be read as a sensitivity check rather than as exact disattenuated values.

Finally, the two polysemy measures are static WordNet proxies rather than corpus-based measures of contextual usage variability such as semantic diversity (Hoffman et al., 2013). WordNet also enumerates senses more finely than psychological or usage-based sense distinctions (Kilgarriff, 1997), so the null effect of synset count may partly reflect a mismatch between lexicographic enumeration and the cross-category sense competition that actually drove norming difficulty here. Future work could substitute corpus-derived diversity measures and treat the WordNet counts as sensitivity checks.

### Conclusion

The benchmark returns a boundary rather than a winner. Embedding regression holds the overall advantage, and the advantage reverses by construct: embeddings are stronger across sensorimotor and abstract content, elicitation where evaluative and hedonic meaning is richly verbalized. The model's response distribution respects the same boundary in both its mean and its spread. Its mean agrees with human norms more closely than its top token on every dimension, and its spread tracks human disagreement only where the construct is itself well recovered; signal follows what is recoverable from language use, and calibration is lost where judgment depends on embodied or developmental experience. Because the expected value is available analytically where log-probabilities are exposed and by sampling where they are not, the distributional reading transfers across model families. The practical rule is short: let the construct choose the representation, and when the representation is an LLM, read its distribution rather than its token. Where that distribution does and does not calibrate to human data is itself a measurement of which lexical knowledge language use carries.


## Data and Code Availability

The integrated lexical resource, norm datasets, and standardized LLM prompt instructions are distributed in the `lexis` R package (Timm, 2025; https://github.com/jaytimm/lexis). A single canonical analysis document, `analysis/paper1/analysis-paper1-multimodel.Rmd` (rendered as `analysis-paper1-multimodel.html`), reproduces every reported statistic — the benchmark, the expected-value and sampling comparisons, the moderation and reliability analyses, the predicted-range calibration, and the logprob-SD analysis. The stratified vocabulary sample, the embedding model predictions and diagnostics, and the raw LLM responses with logprob distributions are in the `analysis/` directory of that repository, with LLM responses under `analysis/output/runs/`; each run carries a `run_meta.json` recording the exact API model snapshot, extraction mode, and parameters, and runs are auto-discovered by the analysis document.

Integer ratings were collected from four models spanning size and provider — gpt-4o-mini and gpt-4.1 (OpenAI; runs `gpt-4o-mini_20260601T171307`, `gpt-4.1_20260601T145807`), claude-haiku-4-5 (Anthropic; `claude-haiku-4-5_20260601T170158`), and gpt-5.4-nano (OpenAI; `gpt-5.4-nano_20260601T154511`) — each queried at temperature = 0 with `max_tokens` = 1 under the standardized prompts. For the two log-probability-capable OpenAI models, the same calls additionally returned `top_logprobs` = 20, from which the logprob-weighted expected values and distributional SDs were computed. The sampling route was collected on gpt-4o-mini as a separate run (`gpt-4o-mini_sampled_20260601T192832`; temperature = 1.0, 20 draws per item via the OpenAI `n` parameter), with all per-draw integers archived. All runs were collected on 2026-06-01/02 (UTC timestamps in each `run_meta.json`). All reported bootstrap confidence intervals use 2,000 resamples under a fixed seed (20260604); confidence intervals on mixed-model fixed effects are 95% Wald intervals. A persistent archive with a citable DOI will be deposited on acceptance [DOI to be added].

---

## References

Aher, G., Arriaga, R. I., & Kalai, A. T. (2023). Using large language models to simulate multiple humans and replicate human subjects studies. *Proceedings of the 40th International Conference on Machine Learning*, 337–371.

Andrews, M., Vigliocco, G., & Vinson, D. (2009). Integrating experiential and distributional data to learn semantic representations. *Psychological Review, 116*(3), 463–498.

Argyle, L. P., Busby, E. C., Fulda, N., Gubler, J. R., Rytting, C., & Wingate, D. (2023). Out of one, many: Using language models to simulate human samples. *Political Analysis, 31*(3), 337–351.

BAAI. (2023). *BGE-large-en-v1.5*. Beijing Academy of Artificial Intelligence. Retrieved from https://huggingface.co/BAAI/bge-large-en-v1.5

Banks, B., Wingfield, C., & Connell, L. (2021). Linguistic distributional knowledge and sensorimotor grounding both contribute to semantic category production. *Cognitive Science, 45*, e13055.


BAAI. (2023). *bge-large-en-v1.5* [Embedding model]. Hugging Face. https://huggingface.co/BAAI/bge-large-en-v1.5
Barsalou, L. W. (1999). Perceptual symbol systems. *Behavioral and Brain Sciences, 22*, 577–660.

Barsalou, L. W. (2008). Grounded cognition. *Annual Review of Psychology, 59*, 617–645.

Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. *Journal of Statistical Software*, *67*(1), 1–48.

Bestgen, Y., & Vincze, N. (2012). Checking and bootstrapping lexical norms by means of word similarity indexes. *Behavior Research Methods, 44*, 998–1006.

Birchenough, J. M. H., Davies, R., & Connelly, V. (2017). Rated age-of-acquisition norms for over 3,200 German words. *Behavior Research Methods, 49*(2), 484–501. https://doi.org/10.3758/s13428-016-0718-0

Brainerd, C. J. (2018). The emotional-ambiguity hypothesis: A large-scale test. *Psychological Science, 29*, 98–115.

Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. *Behavior Research Methods*, *46*, 904–911.

Carlson, R., Bauer, J., & Manning, C. D. (2025). A new pair of GloVes. *arXiv*. https://doi.org/10.48550/arXiv.2507.18103


Clark, J. M., & Paivio, A. (2004). Extensions of the Paivio, Yuille, and Madigan (1968) norms. *Behavior Research Methods, Instruments, & Computers, 36*(3), 371–383.

Conde, J., González, M., Grandury, M., Martínez, G., Reviriego, P., & Brysbaert, M. (2025). Psycholinguistic word features: A new approach for the evaluation of LLMs alignment with humans. *Proceedings of the 4th Generation, Evaluation and Metrics Workshop*. https://arxiv.org/abs/2506.22439

Conde, J., Grandury, M., Fu, T., Arriaga, C., Martínez, G., Clark, T., Trott, S., Green, C. G., Reviriego, P., & Brysbaert, M. (2025). Adding LLMs to the psycholinguistic norming toolbox: A practical guide to getting the most out of human ratings. *arXiv*. https://arxiv.org/abs/2509.14405

Connell, L., & Lynott, D. (2024). What can language models tell us about human cognition? *Current Directions in Psychological Science, 33*, 3–10.

Diveica, V., Pexman, P. M., & Binney, R. J. (2023). Quantifying social semantics: An inclusive definition of socialness and ratings for 8,388 English words. *Behavior Research Methods*, *55*, 461–473.

Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software, 33*(1), 1–22.

Green, C., Kong, A. P.-H., Brysbaert, M., & Keogh, K. (2025). Crowdsourced and AI-generated age-of-acquisition (AoA) norms for vocabulary in print: Extending the Kuperman et al. (2012) norms. *Behavior Research Methods, 57*, 304.

Hagihara, H., & Miyazawa, K. (2026). How well do large language models mirror human cognition of word concepts? A comparison of psychological ratings for early-acquired English words. *Behavior Research Methods, 58*, 58.

Hoffman, P., Lambon Ralph, M. A., & Rogers, T. T. (2013). Semantic diversity: A measure of semantic ambiguity based on variability in the contextual usage of words. *Behavior Research Methods, 45*, 718–730.

Kello, C. T., Bruna, P., & Thao, K. (2025). Contextual assembly of lexical functions in large language models. *Behavior Research Methods, 58*, 19.

Kilgarriff, A. (1997). I don't believe in word senses. *Computers and the Humanities, 31*(2), 91–113.

Kuperman, V., Stadthagen-Gonzalez, H., & Brysbaert, M. (2012). Age-of-acquisition ratings for 30,000 English words. *Behavior Research Methods*, *44*, 978–990.

Kurfali, M., Herman, P., Pierzchajlo, S., Olofsson, J., & Hörberg, T. (2025). Representations of smells: The next frontier for language models? *Cognition, 264*, 106243.


Li, D., Li, L., & Qiu, H. S. (2025). ChatGPT is not a man but Das Man: Representativeness and structural consistency of silicon samples generated by large language models. *arXiv*. https://arxiv.org/abs/2507.02919
Lynott, D., Connell, L., Brysbaert, M., Brand, J., & Carney, J. (2020). The Lancaster Sensorimotor Norms: Multidimensional measures of perceptual and action strength for 40,000 English words. *Behavior Research Methods*, *52*, 1271–1291.

Mandera, P., Keuleers, E., & Brysbaert, M. (2017). Explaining human performance in psycholinguistic tasks with models of semantic similarity based on prediction and counting: A review and empirical validation. *Journal of Memory and Language, 92*, 57–78.

Martínez, G., Molero, J. D., González, S., Conde, J., Brysbaert, M., & Reviriego, P. (2025). Using large language models to estimate features of multi-word expressions: Concreteness, valence, arousal. *Behavior Research Methods, 57*, 5.

Miller, G. A. (1995). WordNet: A lexical database for English. *Communications of the ACM, 38*(11), 39–41.

Muraki, E. J., Born, S., & Pexman, P. M. (2025). Grasping variance in word norms: Individual differences in motor imagery and semantic ratings. *Journal of Cognition, 8*(1), 12. https://doi.org/10.5334/joc.418

Peng, B., Hsu, Y.-y., Chersoni, E., Qiu, L., & Huang, C.-R. (2025). Multilingual prediction of semantic norms with language models: A study on English and Chinese. *Language Resources and Evaluation, 59*, 3911–3937.

Pollock, L. (2018). Statistical and methodological problems with concreteness and other semantic variables: A list memory experiment and a cross-level interaction. *Behavior Research Methods, 50*, 1198–1216.

Recchia, G., & Jones, M. N. (2012). More data trumps smarter algorithms: Comparing pointwise mutual information with latent semantic analysis. *Behavior Research Methods, 44*, 647–665.

Reijnierse, W. G., Burgers, C., Bolognesi, M., & Krennmayr, T. (2019). How polysemy affects concreteness ratings: The case of metaphor. *Cognitive Science, 43*(8).


Scott, G. G., Keitel, A., Becirspahic, M., Yao, B., & Sereno, S. C. (2019). The Glasgow Norms: Ratings of 5,500 words on nine scales. *Behavior Research Methods, 51*(3), 1258–1270.

Timm, J. (2016). *Lexical variation, lexical innovation, and speaker motivations* [Doctoral dissertation, University of New Mexico].

Timm, J. (2025). *lexis: English psycholinguistic norms in a unified tidy resource* (Version 0.1.0) [R package]. GitHub. https://github.com/jaytimm/lexis

Trott, S. (2024a). Can large language models help augment English psycholinguistic datasets? *Behavior Research Methods, 56*, 6082–6100.

Trott, S. (2024b). Large language models and the wisdom of small crowds. *Open Mind, 8*, 723–738.

Utsumi, A. (2020). Exploring what is encoded in distributional word vectors: A neurobiologically motivated analysis. *Cognitive Science, 44*, e12844.

Warriner, A. B., Kuperman, V., & Brysbaert, M. (2013). Norms of valence, arousal, and dominance for 13,915 English lemmas. *Behavior Research Methods*, *45*, 1191–1207.

Winter, B., Perlman, M., & Majid, A. (2023). Iconicity ratings for 14,000+ English words. *Behavior Research Methods*, *55*, 1640–1655.

Xu, Q., Peng, Y., Nastase, S. A., Chodorow, M., Wu, M., & Li, P. (2025). Large language models without grounding recover non-sensorimotor but not sensorimotor features of human concepts. *Nature Human Behaviour*. https://doi.org/10.1038/s41562-025-02203-8

