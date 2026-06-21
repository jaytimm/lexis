# Language Models and Embeddings Capture Different Psycholinguistic Constructs: A Multi-Model Benchmark and Embedding Geometry Probe

**Jay Timm**
*[Institution]*

---

## Abstract

Two computational approaches can approximate psycholinguistic norms at scale: prompt a large language model to rate a word, or train a regression model on word embeddings. We benchmark four LLMs (gpt-4o-mini, gpt-4.1, claude-haiku-4-5, gpt-5.4-nano) against GloVe-300 and BGE-large-en-v1.5 ridge models on a stratified 1,638-word holdout across 18 dimensions, and find the same boundary every time. LLMs win on valence, arousal, age of acquisition, gustatory, and olfactory; embeddings win on sensorimotor and effector dimensions. The boundary tracks what text carries: speakers verbalize evaluation and flavor constantly; no one writes about how much a word involves the torso. The embedding advantage widened as words grew more abstract — the vocabulary that norm extension actually targets, and that convenience samples routinely exclude.

When elicitation fails we ask why. Three explanations are available: the norm is noisy, the signal is absent from text, or the signal is present but not accessible through rating-style generation. Reliability rules out the first — the embedding advantage is largest on high-reliability sensorimotor dimensions. Embedding success where elicitation fails rules out the second. What remains is access: the information is in text; the LLM cannot voice it as a rating.

The axis probe is the punctuation. For each dimension we project holdout words onto a single BGE direction defined by the highest- and lowest-rated training words, fitting no regression weights. On four dimensions — dominance, auditory, head, interoceptive — this unfitted projection outperforms a state-of-the-art LLM (bootstrap CIs exclude zero). The signal was there before any supervised training. Elicitation does not fail because the information is absent. It fails because it cannot be said.

For norm extension, the choice of method should follow the construct.

---

## Plain Language Summary

We ask two computational methods to approximate psycholinguistic norms — one prompts an LLM to rate words, one trains a regression model on word embeddings — and we find the same boundary every time across four LLM families and 18 dimensions: LLMs win on valence, arousal, taste, smell, age of acquisition; embeddings win on everything sensorimotor and effector. The boundary tracks what text carries. Speakers verbalize evaluation and flavor constantly; nobody writes about how much a word involves their torso.

When elicitation fails we ask why, and the framework gives three options: noisy norm, absent signal, or inaccessible signal. Reliability rules out the first; embeddings succeeding where elicitation fails rules out the second. What's left is access — the information is in text, the LLM just can't voice it as a rating.

The probe is the punctuation. We take the embedding geometry and project words onto a single axis defined by the highest- and lowest-rated training words, no regression weights, nothing fitted. On dominance, auditory, and head that unfitted projection beats a state-of-the-art LLM. The signal was there before any supervised training. Elicitation doesn't fail because the information is absent. It fails because it can't be said.

---

## Introduction

Psycholinguistic norms are ratings that quantify properties of words — concreteness, emotional valence, the strength of a word's association with particular senses or body parts. They are collected by averaging the judgments of many human raters and are widely used to study how words are represented and processed. Because each norm requires many raters, published norm sets cover only a fraction of the vocabulary, and extending them by hand is slow and expensive.

What a speaker knows about a word comes from more than one source. On usage-based and grounded-cognition accounts (Barsalou, 1999, 2008; Andrews et al., 2009; Connell & Lynott, 2024), some lexical knowledge is **usage-encoded**: it accumulates from how a word is used in the language, its contexts, collocates, and evaluative coloring, and is in principle recoverable from a sufficiently large corpus. Some is **experience-encoded**: it derives from bodily, perceptual, and developmental experience, which text reflects only indirectly, through description rather than through the experience itself. A speaker rating a word draws on both. A method that learns from text alone has direct access to the usage-encoded part and only indirect access to the experience-encoded part.

Two computational approaches now attempt to approximate human norms at scale, and they inherit this asymmetry differently. In **elicitation**, a large language model is prompted to rate a word as a participant would (Aher et al., 2023; Stevenson et al., 2022; Trott, 2024). Because it produces a judgment in language, it can express what is usage-encoded but has no direct route to knowledge that language never encoded. In **embedding regression**, a supervised model is trained to map a word's embedding onto human ratings (Recchia & Jones, 2012; Bestgen & Vincze, 2012; Mandera et al., 2017; Utsumi, 2020). Because it fits a statistical mapping rather than producing a judgment, it can exploit whatever trace of a construct survives in the distributional signal, even where language encodes it only weakly. Neither approach is a substitute for human raters. The question is where each succeeds, where each fails, and what the pattern reveals about which parts of lexical knowledge text carries.

Three things are reasonably well established about elicitation. LLM ratings correlate substantially with human norms across many constructs (Trott, 2024; Kello et al., 2025; Hagihara & Miyazawa, 2026; Peng et al., 2025). The alignment is uneven: it is strongest for affective and lexical dimensions and weakest for sensorimotor ones, which require translating language back into bodily experience (Trott, 2024; Hagihara & Miyazawa, 2026; Conde et al., 2025; Xu et al., 2025). This unevenness is what grounded-cognition accounts predict — a text-trained model should recover usage-encoded judgments and stumble on experience-encoded ones (Connell & Lynott, 2024).

Two gaps remain. First, the established findings have not been tested on a lexically balanced sample. Prior evaluations use concrete, frequent, well-studied words: early-acquired CDI items (Hagihara & Miyazawa, 2026), polysemy-enriched Glasgow words (Trott, 2024), and a cross-database intersection of common items (Kello et al., 2025). Norm extension targets the opposite vocabulary — low-frequency, late-acquired, abstract words that these samples exclude. A method can perform well on the former and fail silently on the latter, and current evidence cannot distinguish the two cases.

Second, an elicitation failure is uninterpretable on its own. When a model's ratings miss a human norm, three explanations are available. The norm itself may be unreliable — we call this the **noise** mode. The construct may not be recoverable from text by any method — the **difficulty** mode. Or the construct may be present in text yet not accessible through rating-style generation — the **access** mode. These modes have distinct signatures across methods. Noise and difficulty produce convergent failure, in which elicitation and a text-based competitor fail together. Only access produces divergence, in which a method that reaches text differently recovers what elicitation cannot. Separating the modes therefore requires a second method with different text access, evaluated on the same items. Embedding regression serves as that method: where it recovers a construct that elicitation misses, noise and difficulty are excluded and the access mode is isolated. The two methods have not been compared on the same items (cf. Peng et al., 2025), so the access mode has not been directly tested.

Most existing work in this space asks how accurate LLM ratings are and returns a number. We ask a different question: where does each method succeed, where does it fail, and what does the pattern reveal about what text actually carries? We benchmark four LLMs spanning size and provider — gpt-4o-mini, gpt-4.1, claude-haiku-4-5, and gpt-5.4-nano — against GloVe-300 and BGE-large-en-v1.5 ridge models on a stratified 1,638-word holdout spanning concreteness, frequency, and polysemy, across 18 psycholinguistic dimensions. The embedding models are not just a comparison baseline; they are the diagnostic that separates the failure modes. When embedding regression recovers a construct that elicitation cannot, the access mode is identified: the signal was in the text all along.

Because the embedding advantage could alternatively reflect supervised calibration — ridge is trained on thousands of labeled examples, while elicitation is zero-shot — we close with an unfitted BGE axis probe. The probe constructs a single embedding direction from the highest- and lowest-rated training words, fitting no regression weights. Where it outperforms elicitation, the advantage cannot be attributed to label fitting. The signal was in the geometry before any supervision began.

---

## Method

### Norm Dimensions

Eighteen psycholinguistic dimensions were included. Eleven sensorimotor dimensions came from the Lancaster Sensorimotor Norms (Lynott et al., 2020): auditory, visual, haptic, interoceptive, gustatory, olfactory, foot/leg, hand/arm, head, mouth, and torso (scale 0–5). Three affective dimensions came from the Warriner et al. (2013) norms: valence, arousal, and dominance (scale 1–9). Four lexical dimensions were included: concreteness (Brysbaert et al., 2014; scale 1–5), socialness (Diveica et al., 2023; scale 1–7), iconicity (Winter et al., 2023; scale 1–7), and age of acquisition (Kuperman et al., 2012; scale 1–25). Dimensions were selected to span sensorimotor, affective, and lexical content while retaining per-item rater counts and standard deviations — required for the reliability analysis — and sufficient vocabulary overlap for the stratified sample.

### Vocabulary Sample

The eligible pool was restricted to words with ratings across all 18 dimensions and a BGE-large embedding, yielding approximately 6,553 words. A stratified sample was drawn using a hybrid floor-plus-proportional allocation strategy across cells defined by concreteness quintile (5 levels) × Zipf frequency tertile (3 levels) × polysemy band (4 levels: 1 / 2–3 / 4–9 / 10+ WordNet synsets; Miller, 1995). Each occupied cell received a floor allocation of 40 words; remaining budget was distributed proportionally to cell capacity. This design ensured representation across the full lexical space, including low-frequency, abstract, and polysemous words that convenience samples systematically exclude.

Two lexical-ambiguity measures were distinguished. Polysemy was operationalized as WordNet synset count (log-transformed for moderation analyses). Grammatical versatility was operationalized as the number of distinct WordNet parts of speech in which a word has at least one synset (range 1–4). These capture different sources of contextual ambiguity: synset count reflects sense multiplicity; versatility reflects competition across grammatical roles.

The holdout set of 1,638 words (25%) was assigned within strata. Holdout properties: concreteness *M* = 3.13, *SD* = 1.00 (range 1.2–5.0); Zipf frequency *M* = 3.49, *SD* = 0.67 (range 2.5–6.5); WordNet synset count *M* = 3.8, *SD* = 4.2 (range 1–75). All analyses use the holdout only; the training set was used only to fit ridge models and to define probe poles.

### LLM Procedure

LLM ratings were collected from four models: gpt-4o-mini and gpt-4.1 (OpenAI), claude-haiku-4-5 (Anthropic), and gpt-5.4-nano (OpenAI). Each word was presented as a single-turn prompt consisting of the dimension-specific rating instructions followed by the word on a new line, queried at temperature 0 with `max_tokens` = 1. Instructions were drawn from standardized LLM-adapted versions of the original participant instructions for each norming dataset, held constant across models.

The **top-token readout** — the most probable scale token at temperature 0, parsed as an integer — was collected for all four models and anchors the cross-model benchmark. For the two logprob-capable models (gpt-4o-mini, gpt-4.1), the same API call was made with `logprobs = TRUE` and `top_logprobs = 20`, yielding a probability distribution over scale tokens. Because a published norm is a mean over raters, the probability-weighted mean of this distribution is the structurally matched LLM estimate; we use the **expected-value readout** for these two models when comparing their performance directly against embedding regression. Coverage (the share of probability mass on valid scale tokens) was 1.000 across all 18 dimensions.

### Embedding Models

Two embedding models were evaluated: GloVe-300 (Carlson et al., 2025; 300-dimensional updated English GloVe vectors trained on Wikipedia, Gigaword, and a subset of Dolma) and BGE-large-en-v1.5 (BAAI, 2023; 1,024-dimensional sentence transformer trained with contrastive objectives). The two were chosen to bracket the embedding spectrum: GloVe-300 represents the classical static distributional approach; BGE-large-en-v1.5 represents a strong contemporary contrastively trained encoder. For each dimension, ridge regression (5-fold cross-validation on the 4,915 training words, alpha = 0) was fit on the training set and evaluated on the holdout. This follows the established embedding norm-prediction approach (Bestgen & Vincze, 2012; Mandera et al., 2017; Utsumi, 2020).

### Axis Probe

To evaluate whether the embedding advantages reflect access to distributional information or supervised calibration, we scored holdout words with an unfitted BGE axis probe. For each dimension, training words were sorted by their human norm score. The 50 words with the lowest ratings and the 50 words with the highest ratings were identified as scale poles. All BGE-large-en-v1.5 vectors were L2-normalized. The low-pole vectors were averaged to produce a low-pole mean; the high-pole vectors were averaged to produce a high-pole mean. A single axis was defined as the difference vector: axis = high-pole mean − low-pole mean. Each holdout word was then scored by projecting its normalized BGE vector onto this axis (score = word_vector · axis), and scores were correlated with holdout human ratings to yield a per-dimension holdout *r*.

No regression weights are fitted to norm labels; labels are used only to locate the two ends of the scale in the training data. The axis probe is therefore weaker than ridge regression by design. The relevant comparison is probe versus elicitation, not probe versus ridge. When the unfitted probe outperforms a zero-shot LLM judgment, the relevant signal demonstrably exists in embedding geometry before any supervised calibration, and the advantage cannot be attributed to label fitting.

Pole size was set to *n* = 50 as the primary comparison; sensitivity analyses at *n* = 20, 30, and 100 are reported in the companion analysis document and show that the qualitative probe-over-elicitation pattern is stable across the range.

### Statistical Approach

All analyses used the holdout set only (*n* = 1,638). The benchmark compares methods at the dimension level: each of the 18 dimensions contributes one holdout Pearson *r*, and these 18 values are the unit of analysis for dimension-level summaries. This is appropriate because the comparison is between methods across constructs; the construct is the replicate.

For word-level analyses, range-normalized absolute prediction error was used (each absolute error divided by the dimension's response-scale span), so that dimensions on different scales contribute comparably. Mixed-effects models were fit in lme4 (Bates et al., 2015) with REML and crossed random effects for word and dimension; likelihood-ratio tests used ML estimation. Ridge models were fit by 5-fold cross-validation on the training words only (Friedman et al., 2010).

The overall accuracy ordering was confirmed in a mixed-effects model on range-normalized error with method as a fixed factor, using BGE-large as the reference level. Per-dimension differences between gpt-4.1 and BGE-large were assessed with paired bootstraps over words (2,000 resamples, fixed seed), scored on the same resampled items, yielding 95% confidence intervals on the per-dimension advantage.

Word-level moderators were tested by interacting method with concreteness, frequency, log synset count, and grammatical versatility jointly in the mixed-effects error model, with gpt-4.1 versus BGE-large as the comparison; each interaction is estimated controlling for the others. An omnibus likelihood-ratio test assessed the interaction block as a whole before examining individual moderators.

Norm reliability was estimated for each dimension as true-score variance divided by observed between-item variance, treating each item's measurement-error variance as SD²/*n*raters. For the Lancaster norms, which report no per-item rater count, the modality-level *N*known was used. The relationship between the per-dimension embedding advantage and per-dimension reliability was assessed by Pearson correlation across the 18 dimensions. Predicted-range calibration was evaluated by comparing each method's prediction SD to the human norm SD across dimensions.

---

## Results

### Multi-Model Benchmark

We benchmarked four LLMs against GloVe-300 and BGE-large on 18 psycholinguistic dimensions, using the top-token readout for all models.

**Overall ordering.** BGE-large was the most accurate method overall (mean holdout *r* = .734), and GloVe-300 was second (.652). Among the LLMs, gpt-4.1 was strongest (*r* = .636), followed by gpt-4o-mini (.605), claude-haiku-4-5 (.569), and gpt-5.4-nano (.402). In the mixed-effects model on range-normalized error (BGE-large as reference), every LLM showed reliably higher error: GloVe-300 β = +0.059; claude-haiku-4-5 +0.420; gpt-4o-mini +0.434; gpt-4.1 +0.554; gpt-5.4-nano +0.871 (all *p* < .001).

**The construct-dependent pattern.** The overall ranking is not the primary result. BGE-large had the highest *r* on 13 of 18 dimensions, but all four LLMs led on the same cluster of five dimensions: valence, arousal, age of acquisition, gustatory, and olfactory. No LLM led on any sensorimotor effector dimension; embedding regression dominated the abstract and sensorimotor space across all model families.

The pattern is not a model idiosyncrasy. Across all four families, the same cluster favors elicitation and the same sensorimotor dimensions favor embeddings. The models differ in level, not in shape: gpt-5.4-nano collapses on abstract sensorimotor content (auditory *r* = .087, head .081, torso .160) yet still recovers valence (.808), and gpt-4.1 tracks BGE-large most closely without overturning it anywhere on that sensorimotor space. Increasing capability raises agreement across the board but does not change which dimensions favor which method. The boundary follows the kind of lexical knowledge being predicted, not a single model's training run.

**Table 1.** Holdout Pearson *r* by dimension and method (top-token readout; *n* = 1,638). Bold marks the highest *r* in each row. Reliability is per-dimension true-score variance divided by observed between-item variance (see Statistical Approach).

| Group | Dimension | gpt-4o-mini | gpt-4.1 | claude-haiku | gpt-5.4-nano | GloVe-300 | BGE-large | Reliability |
|---|---|---|---|---|---|---|---|---|
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
| **Mean** | | .605 | .636 | .569 | .402 | .652 | **.734** | .823 |

*Note.* All LLM columns use the top-token integer readout; for the two logprob-capable models (gpt-4o-mini, gpt-4.1), expected-value correlations are gpt-4o-mini .628 and gpt-4.1 .646. Group labels organize rows by norm source and are descriptive only. Reliability is estimated from shipped per-item SD and rater counts; for Lancaster norms, *N*known for the corresponding modality was used.

### Lexical Moderators

The construct-dependent pattern in the benchmark has a word-level analogue. An extended mixed-effects model interacted method with concreteness, frequency, log synset count, and grammatical versatility jointly, comparing gpt-4.1 against BGE-large. The omnibus interaction block was highly significant, χ²(4) = 513.7, *p* < .001.

**Concreteness** was the dominant moderator. The method × concreteness interaction (β = +0.018, *t* = 20.4, *p* < .001) shows that the embedding advantage over the strongest LLM was largest for abstract words. Adding concreteness last, over a model already containing the other three interactions, contributed χ²(1) = 413.2, *p* < .001 — much larger than any other individual interaction. The pattern is stable across error metrics and robust to residualization: after removing variance attributable to all four covariates within each dimension, the method ordering was preserved (gpt-4.1 *r*resid = .572, BGE-large .639). The benchmark is not driven by a favorable lexical slice.

**Frequency** showed a reliable but smaller interaction (β = −0.006, *t* = −6.0), roughly a third the magnitude of the concreteness interaction and opposite in sign. Grammatical versatility was reliable (BGE × n_pos β = +0.004, *t* = 3.5) but small. Fine-grained synset count was smaller still (β = +0.007, *t* = 6.1). These secondary moderators indicate modest shifts in the embedding advantage across ambiguity structure, but concreteness is the primary driver.

The abstract-word embedding advantage is where the moderator story lies. Abstract words are where explicit evaluative description thins and meaning is distributed across many contexts — and they are the region that convenience samples never reach, which is why this moderation has gone unobserved.

### Norm Reliability and the Failure-Mode Frame

The construct-dependent pattern in Table 1 needs to be interpreted rather than merely described. Three explanations are possible: differential norm reliability, construct difficulty, or differential access. Reliability analysis and divergence testing separate them.

**Reliability rules out the noise mode.** A noisier published mean caps the achievable correlation for every method equally, so if reliability differences drove the pattern, the embedding advantage should be positively associated with per-dimension reliability — less reliable norms would hurt both methods similarly, not preferentially favor embeddings. The opposite holds: across 18 dimensions, the embedding advantage (BGE − gpt-4.1) was negatively correlated with reliability (*r* = −.33, *p* = .185). The decisive evidence is dimensional: large embedding advantages fall on high-reliability norms. Auditory has reliability .861 and a gap of .413 (gpt-4.1 *r* = .337 vs. BGE .750). Interoceptive has reliability .870 and a gap of .163 (.637 vs. .800). Reliability is also high where elicitation is competitive: gustatory (.934), olfactory (.893), and age of acquisition (.922). A reliability ceiling that is high on both sides of the construct boundary cannot explain why elicitation tracks taste and smell but not auditory or interoceptive strength.

**Divergence rules out the difficulty mode.** If the relevant information were simply absent from text, both methods — which both depend on text — should fail together. They do not: on the dimensions where elicitation fails, embedding regression succeeds. This divergence is what identifies the access mode. The signal is present in the distributional representation; elicitation cannot recover it through rating-style generation.

**The access mode remains.** Together, the two analyses eliminate the alternatives. High reliability on the divergence dimensions rules out noise. Successful embedding regression where elicitation fails rules out difficulty. What remains is access: the relevant information is present in text representations but is not expressible through prompted LLM judgment. The same boundary that maps across constructs in Table 1 maps across the failure-mode logic: the elicitation-favorable cluster (valence, arousal, AoA, gustatory, olfactory) consists of dimensions whose content is carried in how words are used; the elicitation-failure dimensions (sensorimotor, effector) consist of dimensions whose content is enacted but rarely explicitly verbalized.

### Predicted-Range Calibration

Pearson *r* indexes rank agreement but not scale calibration. The two approaches miscalibrate the predicted range in opposite directions, which matters when generated norms are used downstream rather than merely validated.

Ridge regression estimates a conditional mean, so its fitted values are attenuated relative to the truth by approximately the validation correlation. BGE-large's prediction standard deviations averaged .72 of the human norm SD; GloVe-300's averaged .63. Embedding-predicted norms are range-compressed: extreme items are pulled toward the mean, and downstream effect sizes based on predicted scores will be biased toward zero.

Direct elicitation carries no such structural attenuation. For gpt-4.1, the prediction SD averaged 1.35× the human norm SD (1.73× for affective dimensions; 1.42× for lexical; approximately 1.22–1.23× for sensory and effector dimensions). Elicited norms tend to inflate the predicted range rather than compress it.

For ranking purposes, correlation is the right yardstick and embedding regression leads. For building a norm set whose absolute values and distribution will be used, embedding output needs variance correction, whereas direct elicitation does not attenuate.

### Per-Dimension Pattern

The per-dimension bootstrap CIs confirm that the construct-dependent pattern is not sampling artifact. In the paired bootstrap (2,000 resamples, gpt-4.1 vs. BGE-large):

Reliable LLM advantages (bootstrap interval excludes zero in LLM's favor): age of acquisition (BGE − LLM Δ = −.115 [−.138, −.094]), olfactory (−.090 [−.127, −.054]), gustatory (−.056 [−.092, −.024]), arousal (−.054 [−.078, −.031]), valence (−.031 [−.046, −.016]). Haptic was effectively tied (.002 [−.028, .032]).

Reliable BGE advantages (interval excludes zero in embedding's favor): auditory, head, interoceptive, mouth, torso, foot/leg, hand/arm, visual, dominance, iconicity, concreteness, socialness.

The verdict is graded: a few reliable elicitation wins concentrated on evaluative and experience-linked dimensions where content is richly verbalized, against a broad and consistent embedding advantage across sensorimotor space.

### Axis Probe: Supervision-Free Embedding Advantage

The divergence pattern — embeddings succeed where elicitation fails — is what identifies the access mode. But a reviewable objection remains: ridge regression is trained on thousands of labeled examples, while elicitation is zero-shot. A supervised model can beat a zero-shot system without revealing anything about representational access, if the advantage reflects label fitting rather than information in the geometry.

The axis probe addresses this directly. It uses embedding geometry with no fitted regression weights. Dimension by dimension, a single axis is defined by the difference between the mean vector of the highest-rated training words and the mean vector of the lowest-rated training words, and holdout words are scored by projecting onto that axis. If the probe outperforms elicitation, the relevant signal was demonstrably present in the embedding geometry before any supervision.

**Table 2.** Axis probe (*n* = 50 poles) compared with gpt-4.1 top-token elicitation and BGE-large ridge, for dimensions where the unfitted probe outperformed elicitation. Dimensions are ordered by magnitude of probe advantage.

| Dimension | BGE axis *r* | gpt-4.1 top-token *r* | BGE ridge *r* |
|---|---|---|---|
| dominance | .701 | .250 | .742 |
| auditory | .623 | .337 | .750 |
| head | .480 | .257 | .628 |
| interoceptive | .701 | .637 | .800 |
| mouth | .616 | .594 | .728 |
| torso | .566 | .523 | .709 |

*Note.* The probe uses no fitted regression weights; norm labels locate the scale poles only. The probe is expected to underperform ridge (ridge fits a supervised mapping). The relevant comparison is probe vs. elicitation. Paired bootstrap CIs (2,000 resamples, seed 20260604): dominance Δ = .451 [.393, .512]; auditory Δ = .286 [.238, .336]; head Δ = .222 [.164, .284]; interoceptive Δ = .064 [.033, .095]; torso Δ = .042 [−.007, .092]; mouth Δ = .022 [−.016, .063]. Four dimensions clear zero; torso and mouth do not.

On several dimensions, a one-dimensional, unfitted geometric projection outperformed a state-of-the-art prompted language model. Dominance and auditory are the most striking: the probe's advantage over elicitation exceeds .40 in each case, and interoceptive and head exceed .06 and .22, respectively. The fact that the probe is deliberately weaker than ridge — it uses one dimension with no fitting — makes these results diagnostic. There is no plausible mechanism by which an unfitted projection could outperform a well-calibrated LLM except that the relevant signal was already organized along a coherent direction in the embedding space.

Across the remaining 12 dimensions, the probe did not outperform elicitation. The probe-over-elicitation dimensions are a subset of the embedding-over-elicitation dimensions; the probe decomposes the embedding advantage into supervision-independent and supervision-dependent cases. Some embedding advantages clearly survive without supervision; others require the fitting that ridge provides, suggesting the relevant signal is present but distributed across multiple dimensions rather than concentrated along a single axis.

---

## Discussion

### Which Constructs Are Recoverable from Text

The leaderboard answer — BGE-large wins, mean *r* = .734 — is the least interesting finding here. What matters is that the pattern is construct-dependent and structurally stable: the same five dimensions favor every LLM across four families, and the same sensorimotor space goes to embeddings every time. That is not a model result; it is a result about the constructs. The elicitation-favorable cluster (valence, arousal, age of acquisition, gustatory, olfactory) consists of dimensions whose content is heavily and routinely verbalized in evaluative and experiential terms. Words are used in contexts that communicate positivity, unpleasantness, when a word is typically learned, or shared impressions of flavor and smell; a prompted LLM can use that signal directly. The embedding-favorable dimensions (sensorimotor effector dimensions, dominance, interoceptive) are constructs whose content is enacted or experienced but rarely explicitly described; their traces in text are diffuse and require the implicit statistical mapping that supervised regression provides.

The two-source account says why. A human rater draws on usage-encoded and experience-encoded knowledge; both machine routes have only usage-encoded text. Elicitation requires converting a word into an explicit scalar judgment; only constructs that speakers routinely express as evaluations or experiential summaries survive that conversion. Embedding regression needs the relevant structure merely to be somewhere in the vector, accumulated implicitly across contexts; it can exploit diffuse traces that explicit judgment cannot voice. On this reading the embedding advantage is not evidence that embeddings are grounded; it is evidence that regression over continuous representations can exploit weak distributional traces that prompted generation cannot access.

The pattern is reinforced by the moderator analysis. The embedding advantage concentrates on abstract words — the lexical region where explicit evaluative description thins and meaning is carried across diverse contexts — and this moderation has gone unobserved because existing convenience samples do not reach abstract, low-frequency, and polysemous words. Stratified sampling is not a methodological nicety here; it is what makes the construct-level and word-level patterns visible.

### Why the Evaluative and Experience-Linked Cluster Favors Elicitation

Valence, arousal, gustatory, olfactory, and age of acquisition are the dimensions where every LLM leads. These are not generically easy: gustatory and olfactory are floor-distributed (83% and 76% of holdout words at floor), making them closer to sparse detection tasks than to graded estimation. Yet LLMs lead on both, while failing on other floor-leaning effector dimensions such as torso and haptic. Sparseness is one contributor to the dimension-specific pattern, not its primary cause.

The more principled explanation is that gustatory and olfactory are evaluative channels in ordinary text. In review genres centered on food, drink, and fragrance, taste and smell are expressed through pleasantness, disgust, sweetness, freshness, and intensity — hedonic descriptions that are heavily usage-encoded. LLMs benefit from this evaluative register. Sensorimotor effector dimensions, by contrast, ask about bodily involvement (how much does a word engage the *torso*, *hand/arm*, *head*?), a question that speakers rarely answer in text; the trace is distributed and weak. Kurfali et al. (2025) show the same split for olfaction directly: language models perform poorly on non-verbal perceptual odor similarity but substantially better on linguistically mediated odor judgments.

Age of acquisition broadens the interpretation. It is not an affect rating, yet it falls in the elicitation-favorable cluster. Clark and Paivio (2004) provide a mechanism: in their factor analysis, AoA loaded across familiarity, length, concreteness, and frequency, and they suggest that raters judging acquisition age draw on accessible distributional cues rather than autobiographical memory. If AoA ratings are composite judgments built from text-accessible properties, their place in the elicitation-favorable cluster is predicted rather than anomalous.

Dominance is the notable outlier in the opposite direction: the LLM does poorly (*r* = .250) and the probe does well (.701). Dominance is semantically structured in text — words differ systematically in how authoritative, controlled, or powerful they feel — but that structure appears to be organized as a distributional pattern that embedding geometry captures cleanly, while elicitation yields a poorly calibrated rating. This may reflect the abstractness of the dominance construct: speakers do not routinely rate words on a dominant-submissive scale, so there is no verbalized evaluative tradition for the LLM to draw on, even though the construct leaves a strong geometric trace.

### The Probe and What It Establishes

The axis probe was designed to answer one question: do the embedding advantages reflect access to distributional information, or supervised calibration? On several dimensions, the answer is clearly access.

On four dimensions — dominance, auditory, head, interoceptive — the unfitted one-dimensional projection outperforms a state-of-the-art prompted LLM, with bootstrap confidence intervals that exclude zero (dominance Δ = .451 [.393, .512]; auditory Δ = .286 [.238, .336]; head Δ = .222 [.164, .284]; interoceptive Δ = .064 [.033, .095]). There is no mechanism by which this can be explained as label fitting: the probe has no labels. What it has is a direction in BGE-large's embedding space that is coherent with the human norm before any supervised training. The signal is there; elicitation is not accessing it.

The probe decomposes the 13-dimension embedding advantage into two parts. On a subset, the advantage survives without supervision — these are the cases where the failure-mode interpretation is strongest. On the remaining embedding-favorable dimensions, the probe does not outperform elicitation, suggesting either that the relevant signal is multidimensional (requiring regression to aggregate) or that it is weaker and needs supervised calibration to emerge. The probe does not establish access for the full 13-dimension set; it strengthens the access argument specifically where the unfitted result holds.

The symmetry argument provides additional evidence that supervision is not the primary driver of divergence. Ridge regression was supervised on all 18 dimensions simultaneously, yet it lost on five. If supervision were the main mechanism, embeddings should win everywhere they are trained — i.e., everywhere. They do not. The construct-dependent pattern tracks what the constructs are, not the presence of supervision.

The probe is upside-only by design. Where it wins, the access interpretation is strengthened. Where it fails to outperform elicitation, the access interpretation is not disproven — the probe is a weak test, and passage requires that the construct be organized along a single axis in the geometry. Some access-mode constructs may be multidimensionally organized in a way the one-dimensional probe cannot detect.

### Implications

The results carry two practical implications.

For the choice of method, the construct is the guide. For affective, hedonic, and experience-linked dimensions whose content is heavily verbalized — valence, arousal, gustatory, olfactory, age of acquisition — LLM elicitation using the expected-value readout is competitive with or better than embedding regression. For sensorimotor, effector, and abstract dimensions where meaning is enacted rather than verbalized, supervised embedding regression is preferable. One caveat tempers the embedding recommendation: ridge regression attenuates the predicted range (predicted SD ≈ .72 of the human SD for BGE-large), so when a norm set's absolute values and spread will be used downstream rather than only its rankings, embedding output needs variance correction.

For the lexical composition of the norming sample, stratification is a precondition for generalizable conclusions. Samples biased toward high-frequency, concrete, monosemous words will overestimate LLM accuracy and underestimate the embedding advantage. The moderator analyses here would be invisible in such samples. The vocabulary that norm extension actually targets — low-frequency, late-acquired, abstract — is the vocabulary that convenience samples leave out.

### Limitations

The benchmark spans four LLMs across two providers, which addresses the single-model concern: the construct-dependent pattern and the overall accuracy ordering replicate across all four families. LLM papers nonetheless study moving targets. We design the claims around invariants rather than leaderboard numbers: the specific correlations have a short half-life, but the structural argument — which constructs are recoverable through which computational route — should be more stable. The capability gradient across models is a space-for-time check on the boundary, and pinned model IDs, raw responses, and archived runs ensure the reported numbers remain auditable.

The probe establishes four clean wins (bootstrap CIs exclude zero): dominance, auditory, head, interoceptive. Torso and mouth show positive point estimates but intervals that include zero, so those remain suggestive rather than confirmed. The four-dimension clean count is the appropriate basis for inference.

The probe does not establish a positive mechanism for why elicitation fails where it fails. Showing that the signal is in the geometry is an identification result, not an explanation. The grounded-cognition account — that text-trained models can voice usage-encoded content but lack a route to experience-encoded content — remains an interpretation consistent with the data, not a result the probe directly demonstrates.

Prompt instructions were standardized across models and were not re-tuned per model, so cross-model differences reflect the models under a common prompt rather than each model's best-case elicitation. Different instruction formulations could shift performance, particularly on the Lancaster body-part scales, where judging how much a word involves `foot/leg` or `torso` is an unusual metalinguistic task.

The human norms are generation-stamped: Warriner et al.'s raters worked around 2012, Lancaster's around 2019, while the models were trained on text with a later center of mass. Some residual misalignment is expected even on well-recovered dimensions, concentrated on words whose usage has shifted. The available anchor — imagery and familiarity ratings collected three decades apart correlate at .80–.85 (Clark & Paivio, 2004) — suggests exact agreement with a fixed norm is not the right target. Norm age is a residual confound the design does not isolate.

WordNet synset counts and POS counts are static lexicographic proxies, not corpus-based measures of contextual variability. The null effect of synset count may partly reflect the mismatch between lexicographic sense enumeration and the cross-category sense competition that actually modulates norming difficulty.

### Conclusion

The benchmark returns a boundary rather than a winner. Embedding regression holds the overall advantage, and the advantage is construct-dependent: embeddings are stronger for sensorimotor and abstract dimensions where meaning is enacted rather than verbalized, elicitation stronger where evaluative and hedonic content is richly encoded in language use. The pattern holds from the weakest model to the strongest across four LLM families, implicating the two computational routes rather than any particular system.

The failure-mode analysis localizes the elicitation gaps as access failures. Norm reliability rules out noise on the divergence dimensions; the embedding success that defines divergence rules out difficulty. What remains is access: the relevant distributional information exists in text representations but is not recoverable through prompted rating-style generation.

The axis probe gives this interpretation its firmest empirical ground. On several dimensions, an unfitted one-dimensional embedding projection outperforms a state-of-the-art LLM. The signal is present in the geometry before any supervised training. Elicitation fails not because the information is absent, but because it cannot be voiced as a rating.

---

## Data and Code Availability

The integrated lexical resource, norm datasets, and standardized LLM prompt instructions are distributed in the `lexis` R package (Timm, 2025; https://github.com/jaytimm/lexis). The canonical analysis document `analysis/paper1/analysis-paper1-multimodel.Rmd` reproduces all benchmark, moderation, reliability, and predicted-range statistics. The axis probe is implemented in `analysis/paper1/failure-mode-axis-probe.Rmd` with results in `analysis/output/failure_mode_axis_probe_focus.csv`. All LLM responses are archived under `analysis/output/runs/`, with each run's `run_meta.json` recording the model snapshot, extraction mode, and parameters.

Integer ratings: gpt-4o-mini (`gpt-4o-mini_20260601T171307`), gpt-4.1 (`gpt-4.1_20260601T145807`), claude-haiku-4-5 (`claude-haiku-4-5_20260601T170158`), gpt-5.4-nano (`gpt-5.4-nano_20260601T154511`); temperature = 0, `max_tokens` = 1. The two logprob-capable models additionally returned `top_logprobs` = 20. All runs were collected 2026-06-01/02. Bootstrap confidence intervals use 2,000 resamples under a fixed seed (20260604); mixed-model CIs are 95% Wald intervals. A citable DOI will be deposited on acceptance.

---

## References

Aher, G., Arriaga, R. I., & Kalai, A. T. (2023). Using large language models to simulate multiple humans and replicate human subjects studies. *Proceedings of the 40th International Conference on Machine Learning*, 337–371.

Andrews, M., Vigliocco, G., & Vinson, D. (2009). Integrating experiential and distributional data to learn semantic representations. *Psychological Review, 116*(3), 463–498.

BAAI. (2023). *BGE-large-en-v1.5* [Embedding model]. Hugging Face. https://huggingface.co/BAAI/bge-large-en-v1.5

Banks, B., Wingfield, C., & Connell, L. (2021). Linguistic distributional knowledge and sensorimotor grounding both contribute to semantic category production. *Cognitive Science, 45*, e13055.

Barsalou, L. W. (1999). Perceptual symbol systems. *Behavioral and Brain Sciences, 22*, 577–660.

Barsalou, L. W. (2008). Grounded cognition. *Annual Review of Psychology, 59*, 617–645.

Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. *Journal of Statistical Software*, *67*(1), 1–48.

Bestgen, Y., & Vincze, N. (2012). Checking and bootstrapping lexical norms by means of word similarity indexes. *Behavior Research Methods, 44*, 998–1006.

Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. *Behavior Research Methods*, *46*, 904–911.

Carlson, R., Bauer, J., & Manning, C. D. (2025). A new pair of GloVes. *arXiv*. https://doi.org/10.48550/arXiv.2507.18103

Clark, J. M., & Paivio, A. (2004). Extensions of the Paivio, Yuille, and Madigan (1968) norms. *Behavior Research Methods, Instruments, & Computers, 36*(3), 371–383.

Conde, J., González, M., Grandury, M., Martínez, G., Reviriego, P., & Brysbaert, M. (2025). Psycholinguistic word features: A new approach for the evaluation of LLMs alignment with humans. *Proceedings of the 4th Generation, Evaluation and Metrics Workshop*.

Conde, J., Grandury, M., Fu, T., Arriaga, C., Martínez, G., Clark, T., Trott, S., Green, C. G., Reviriego, P., & Brysbaert, M. (2025). Adding LLMs to the psycholinguistic norming toolbox: A practical guide to getting the most out of human ratings. *arXiv*. https://arxiv.org/abs/2509.14405

Connell, L., & Lynott, D. (2024). What can language models tell us about human cognition? *Current Directions in Psychological Science, 33*, 3–10.

Diveica, V., Pexman, P. M., & Binney, R. J. (2023). Quantifying social semantics: An inclusive definition of socialness and ratings for 8,388 English words. *Behavior Research Methods*, *55*, 461–473.

Friedman, J., Hastie, T., & Tibshirani, R. (2010). Regularization paths for generalized linear models via coordinate descent. *Journal of Statistical Software, 33*(1), 1–22.

Green, C., Kong, A. P.-H., Brysbaert, M., & Keogh, K. (2025). Crowdsourced and AI-generated age-of-acquisition (AoA) norms for vocabulary in print. *Behavior Research Methods, 57*, 304.

Hagihara, H., & Miyazawa, K. (2026). How well do large language models mirror human cognition of word concepts? *Behavior Research Methods, 58*, 58.

Hoffman, P., Lambon Ralph, M. A., & Rogers, T. T. (2013). Semantic diversity: A measure of semantic ambiguity based on variability in the contextual usage of words. *Behavior Research Methods, 45*, 718–730.

Kello, C. T., Bruna, P., & Thao, K. (2025). Contextual assembly of lexical functions in large language models. *Behavior Research Methods, 58*, 19.

Kilgarriff, A. (1997). I don't believe in word senses. *Computers and the Humanities, 31*(2), 91–113.

Kuperman, V., Stadthagen-Gonzalez, H., & Brysbaert, M. (2012). Age-of-acquisition ratings for 30,000 English words. *Behavior Research Methods*, *44*, 978–990.

Kurfali, M., Herman, P., Pierzchajlo, S., Olofsson, J., & Hörberg, T. (2025). Representations of smells: The next frontier for language models? *Cognition, 264*, 106243.

Lynott, D., Connell, L., Brysbaert, M., Brand, J., & Carney, J. (2020). The Lancaster Sensorimotor Norms: Multidimensional measures of perceptual and action strength for 40,000 English words. *Behavior Research Methods*, *52*, 1271–1291.

Mandera, P., Keuleers, E., & Brysbaert, M. (2017). Explaining human performance in psycholinguistic tasks with models of semantic similarity based on prediction and counting. *Journal of Memory and Language, 92*, 57–78.

Martínez, G., Molero, J. D., González, S., Conde, J., Brysbaert, M., & Reviriego, P. (2025). Using large language models to estimate features of multi-word expressions. *Behavior Research Methods, 57*, 5.

Miller, G. A. (1995). WordNet: A lexical database for English. *Communications of the ACM, 38*(11), 39–41.

Peng, B., Hsu, Y.-y., Chersoni, E., Qiu, L., & Huang, C.-R. (2025). Multilingual prediction of semantic norms with language models. *Language Resources and Evaluation, 59*, 3911–3937.

Pollock, L. (2018). Statistical and methodological problems with concreteness and other semantic variables. *Behavior Research Methods, 50*, 1198–1216.

Recchia, G., & Jones, M. N. (2012). More data trumps smarter algorithms: Comparing pointwise mutual information with latent semantic analysis. *Behavior Research Methods, 44*, 647–665.

Reijnierse, W. G., Burgers, C., Bolognesi, M., & Krennmayr, T. (2019). How polysemy affects concreteness ratings: The case of metaphor. *Cognitive Science, 43*(8).

Stevenson, C., ter Veen, M., Choenni, R., van der Maas, H. L. J., & Shutova, E. (2022). Do large language models know what humans know? *Cognitive Science, 46*(7), e13190.

Timm, J. (2016). *Lexical variation, lexical innovation, and speaker motivations* [Doctoral dissertation, University of New Mexico].

Timm, J. (2025). *lexis: English psycholinguistic norms in a unified tidy resource* (Version 0.1.0) [R package]. GitHub. https://github.com/jaytimm/lexis

Trott, S. (2024a). Can large language models help augment English psycholinguistic datasets? *Behavior Research Methods, 56*, 6082–6100.

Trott, S. (2024b). Large language models and the wisdom of small crowds. *Open Mind, 8*, 723–738.

Utsumi, A. (2020). Exploring what is encoded in distributional word vectors: A neurobiologically motivated analysis. *Cognitive Science, 44*, e12844.

Warriner, A. B., Kuperman, V., & Brysbaert, M. (2013). Norms of valence, arousal, and dominance for 13,915 English lemmas. *Behavior Research Methods*, *45*, 1191–1207.

Winter, B., Perlman, M., & Majid, A. (2023). Iconicity ratings for 14,000+ English words. *Behavior Research Methods*, *55*, 1640–1655.

Xu, Q., Peng, Y., Nastase, S. A., Chodorow, M., Wu, M., & Li, P. (2025). Large language models without grounding recover non-sensorimotor but not sensorimotor features of human concepts. *Nature Human Behaviour*. https://doi.org/10.1038/s41562-025-02203-8
