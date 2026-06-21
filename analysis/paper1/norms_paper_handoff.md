# Norms Paper — Working Handoff

Consolidated record of decisions and drafted text. Status flags:

- **SETTLED** — agreed framing/wording, treat as current.
- **DRAFT** — written and in good shape, not yet line-final.
- **OPEN** — flagged issue still unresolved; do not treat as final.

---

## 1. Study frame (the spine)

**SETTLED.**

The paper is *one* argument, not two parallel findings.

- **Primary finding (the representation comparison).** Lexical knowledge has more than one source. Two computational methods — LLM **elicitation** and **embedding regression** — have different access to those sources. Which method better approximates a human norm depends on the construct. Elicitation leads on a cluster of evaluative and experience-linked dimensions (valence, arousal, age of acquisition, gustatory, olfactory); embedding regression leads on abstract and sensorimotor dimensions, and its advantage widens as words grow more abstract.

- **The embedding model is a control, not a competitor.** Its job is to determine *why* elicitation fails where it fails. See the failure-mode frame (§3).

- **Supporting findings (reading the distribution).** Two distinct, smaller results, explicitly *not* welded into the primary boundary:
  1. The **expected-value readout** tracks human norms more closely than the **top-token readout** (small, reliable, lexically uniform — a measurement refinement, not a new representation).
  2. The **response-distribution spread** covaries with human inter-rater disagreement, *selectively* on semantically rich dimensions. This is a **different cut** through the dimensions than the primary boundary — related, not identical — and is therefore supporting, never co-headline.

- **Sampling closes Part B.** Both the expected value and the spread are recoverable by the **sampled readout** without log-probabilities, so the entire distributional analysis carries to models that withhold logprobs (GPT-5 series, all Claude models).

**Two-part Results structure:**

- **Part A (primary): the representation comparison.** Multi-model benchmark → isolating the access mode (the control analyses) → lexical moderators → graded verdict.
- **Part B (supporting): reading the distribution.** Extraction question → spread question → sampling closer.

---

## 2. Language contract (frozen vocabulary)

**SETTLED.** Every result sentence speaks to exactly one axis. Naming the axes is what stops mid-clause drift.

### The three axes

| Axis | What it contrasts | Frozen terms |
| --- | --- | --- |
| Representation | how the norm is approximated | **elicitation** (LLM rates as a participant) vs. **embedding regression** |
| Extraction | how a number is read from one LLM query | **top-token readout** / **expected-value readout** / **sampled readout** |
| Distribution property | which property of the LLM's output | **mean** vs. **spread** |

### The two sources of lexical knowledge

- **usage-encoded** — accumulates from how a word is used in the language (contexts, collocates, evaluative coloring); recoverable from a large corpus.
- **experience-encoded** — derives from bodily, perceptual, developmental experience; text reflects it only indirectly.
- Anchor phrase: *"the two sources of lexical knowledge."* This is conceptual load-bearing, not a flourish.

### The readouts

Three ways to read one query. Frozen names above. Key relationships:

- The **top-token readout** is a point estimate that discards the rest of the distribution.
- The **expected-value readout** (analytic, needs log-probabilities) and the **sampled readout** (Monte-Carlo, needs only repeated draws) are **two estimators of one quantity** — the model's probability-weighted mean. Not rivals.
- A human norm is a mean over raters, so the probability-weighted mean is the like-for-like target; the top-token integer is the coarsened one.
- "Expected value" names the **quantity**; "expected-value readout" names the **method**. Do not let "expected value" refer to both in one sentence.

### The failure-mode frame (the engine of Part A)

An elicitation miss has three **failure modes**, each a *prediction* about the joint pattern of the two methods:

| Methods | Norm reliability | Failure mode |
| --- | --- | --- |
| **diverge** (embeddings recover what elicitation misses) | (not needed) | **access** — construct is in text but not expressible as a rating |
| **converge** (both fail) | low | **noise** — the published norm is unreliable |
| **converge** (both fail) | high | **difficulty** — construct is not recoverable from text by any method |

- **convergent failure** = both methods fail (signature of noise *or* difficulty).
- **divergence** = embeddings succeed where elicitation fails (signature of access).
- It takes **two observations** to resolve three modes: divergence-vs-convergence splits *access* off from the other two; reliability splits *noise* from *difficulty* within the convergent branch.
- Reliability is **per dimension** and is used as a **ceiling/bound**, never binarized at a threshold. The headline (divergence) cases sit on high-reliability dimensions, so noise is excluded for those specific dimensions by their specific (high) reliability.
- Mapping to the control analyses: **reliability rules out the noise mode; embedding success (divergence) rules out the difficulty mode; the access mode remains.**

### Logprob-capable models

Defined once: the two OpenAI models (gpt-4o-mini, gpt-4.1). Reuse the phrase. State the withhold-logprobs fact **once** in Methods; never re-explain it per section.

### Kill list

- "reversal" / "flips" / "swaps" / "crossover" / "division of labor" → describe the construct-dependent pattern in plain words; **do not** name it. (All overclaim symmetry the data lacks.)
- "moment" / "first moment" / "second moment" → **mean** and **spread** only.
- "experiential" as the LLM-cluster label → collides with experience-encoded; use **evaluative / evaluative–lexical**.
- "spread" for the across-word range of predictions (Part A) → that is **dynamic range** / **predicted range**; "spread" is reserved for the within-query token distribution (Part B).
- Rhetorical fragments ("It did.", "Only the first half has been tested.").
- Em-dash pileups (cap ~one per sentence; a parenthesized list is fine).
- "secondhand" / "second-hand."
- Repeated logprob-availability caveats.
- Figurative register ("easy corner of the lexicon," "rise and fall with," limp passives like "is wanted," "taken from," vague "what X means").

### Dropped from scope

- **socialness** — removed (crowdsourced ~50% in India; also the coverage bottleneck). Lets the paper say these are **American-English norms**.
- **n_pos / grammatical versatility** — demote to a robustness footnote (1–4 count hard to defend as a distribution).
- **lexis package** — de-emphasize in the vocabulary section; do not foreground.

---

## 3. Abstract

**DRAFT.**

Psycholinguistic norms are increasingly approximated by computational methods, either by eliciting ratings from large language models or by regressing human norms on word embeddings. These approaches are rarely compared on the same items, and existing evaluations rely on small convenience samples skewed toward concrete, frequent words. We benchmark four LLMs (gpt-4o-mini, gpt-4.1, claude-haiku-4-5, gpt-5.4-nano) against GloVe-300 and BGE-large-en-v1.5 ridge models on a stratified 1,638-word holdout across 18 dimensions.

Accuracy depended on the construct. BGE-large was most accurate overall (mean *r* = .734, best on 13 of 18 dimensions), but every LLM led on the same five dimensions — valence, arousal, age of acquisition, gustatory, and olfactory — while embeddings dominated abstract and sensorimotor items. The embedding model recovered the dimensions elicitation missed, showing that those failures reflect what language expresses rather than measurement noise or difficulty. The advantage of embeddings over LLMs widened as words grew more abstract.

Reading the LLM's full response distribution rather than its top token improved alignment further. The probability-weighted mean outperformed the top token on all 18 dimensions, and averaging a small number of temperature-1.0 samples recovered the same value (*r* = .997), extending the method to models that withhold log-probabilities. The distribution's spread tracked human inter-rater disagreement, selectively on semantically rich dimensions.

These results suggest that the choice of method should follow the construct: LLM elicitation for dimensions grounded in language use, supervised embeddings for those grounded in direct experience.

> Note: abstract currently uses one em-dash pair (the five-dimension list). Convert to a colon list if the venue is strict. Optional fuller control clause if space allows: *"Noise or difficulty would have depressed both methods; instead they diverge, which locates the failure in what language expresses."*

---

## 4. Introduction

### 4.1 Opening (norms → motivation)

**OPEN** — paragraph ordering unresolved. The live problem: naming the two methods early creates a *methods → speaker-knowledge → methods* subject bounce across paragraphs. Two candidate fixes, not yet chosen:

- **Option A:** name methods in P1; build an explicit bridge so the speaker-knowledge paragraph reads as deliberate premise, not interruption.
- **Option B (leaning):** hold the methods until P3; P1 ends on the *problem* (hand-collection doesn't scale), not a vague "this motivated approaches"; P2 = speaker knowledge; P3 = the two methods against that premise. World → premise → methods, no bounce. Cost: methods not named until P3.

Best-available P1 (pending the above):

> Psycholinguistic norms are ratings that quantify properties of words, such as concreteness, emotional valence, or the strength of a word's association with the senses. They are collected by averaging the judgments of many human raters and are widely used to study how words are represented and processed. Because each norm requires many raters, published norm sets cover only a portion of the vocabulary, and extending them to new words by hand is slow and expensive.

### 4.2 The two sources of lexical knowledge

**DRAFT.**

> What a speaker knows about a word comes from more than one source. On usage-based and grounded-cognition accounts (Barsalou, 1999, 2008; Andrews et al., 2009; Connell & Lynott, 2024), some lexical knowledge is **usage-encoded**: it accumulates from how a word is used in the language, its contexts, collocates, and evaluative coloring, and is in principle recoverable from a sufficiently large corpus. Some is **experience-encoded**: it derives from bodily, perceptual, and developmental experience, which text reflects only indirectly, through description rather than through the experience itself. A speaker rating a word draws on both. A method that learns from text alone has direct access to the usage-encoded part and only indirect access to the experience-encoded part.

> Note: "more than one source" deliberately avoids claiming the two are exhaustive. Opener is a clean cognition claim with no model/text content; the text-access consequence lands only in the final sentence.

### 4.3 The two approaches

**DRAFT.**

> The two approaches inherit this asymmetry differently, because they use text in different ways. In **elicitation**, a large language model is prompted to rate a word as a participant would (Aher et al., 2023; Stevenson et al., 2022; Trott, 2024). Because it produces a judgment in language, it can express what is usage-encoded but has no route to knowledge that language never encoded. In **embedding regression**, a supervised model is trained to map a word's embedding onto human ratings (Recchia & Jones, 2012; Bestgen & Vincze, 2012; Mandera et al., 2017; Utsumi, 2020). Because it fits a statistical mapping rather than producing a judgment, it can exploit whatever trace of a construct survives in the distributional signal, even where language encodes it only weakly. Neither approach is a substitute for human raters. The question is where each succeeds, where each fails, and what the pattern reveals about which parts of lexical knowledge text carries.

### 4.4 What is established

**DRAFT** (carried from original, lightly held — verify findings/citations).

> Three things are reasonably well established about elicitation. LLM ratings correlate substantially with human norms across many constructs (Trott, 2024; Kello et al., 2025; Green et al., 2025; Hagihara & Miyazawa, 2026; Peng et al., 2025). The alignment is uneven: it is strongest for affective and lexical dimensions and weakest for sensorimotor ones, which require translating language back into bodily experience (Trott, 2024; Hagihara & Miyazawa, 2026; Conde et al., 2025; Xu et al., 2025). This unevenness is what a grounded account predicts — a text-trained model should recover usage-encoded judgments and stumble on experience-encoded ones (Connell & Lynott, 2024).

> OPEN: original draft flags that actual findings should be listed here, not just asserted.

### 4.5 The gaps

**DRAFT.**

> What remains unsettled falls into four areas.
>
> First, the established findings have not been tested on a lexically balanced sample. Prior evaluations use concrete, frequent, well-studied words: early-acquired CDI items (Hagihara & Miyazawa, 2026), polysemy-enriched Glasgow words (Trott, 2024), and a cross-database intersection of common items (Kello et al., 2025). Norm extension targets the opposite vocabulary, the low-frequency, late-acquired, and abstract words these samples exclude. A method can perform well on the former and fail on the latter, and current evidence cannot distinguish the two cases.
>
> Second, an elicitation failure is uninterpretable on its own. When a model's ratings miss a human norm, the miss can arise in three ways. The norm itself may be unreliable, so that no method could track it; we call this the **noise** mode. The construct may not be recoverable from text by any method, the **difficulty** mode. Or the construct may be present in text yet not expressible as a rating, the **access** mode. These modes have distinct signatures across methods. Noise and difficulty both produce convergent failure, in which elicitation and a text-based competitor fail together. Only an access failure produces divergence, in which a method that reaches text differently recovers what elicitation cannot. Separating the modes therefore requires a second method with different access to text, evaluated on the same items. Embedding regression is that method: where it recovers a construct that elicitation misses, noise and difficulty are excluded and the access mode is isolated. The two methods have not been compared on the same items (cf. Peng et al., 2025), so the access mode has never been tested directly.
>
> Third, the extraction method has not been compared across readouts or tested on current models. A human norm is a mean over raters, whereas the standard procedure records the single most probable token at temperature zero, a point estimate. When a model exposes token log-probabilities, the probability-weighted mean of its response distribution aligns more closely with human norms (Argyle et al., 2023; Martínez et al., 2025; Conde et al., 2025). This result comes almost entirely from large proprietary models, and it depends on log-probabilities that several current families withhold, including the GPT-5 series and all Claude models. It therefore cannot be applied to many of the models now in use.
>
> Fourth, it is unknown whether the model's response distribution carries information beyond its mean. Published norms report a standard deviation as well as a mean, because raters disagree, and disagree more on some words than others. A single query yields a distribution over scale tokens, with its own mean and spread. If that distribution behaves like a sample of raters, its spread should covary with human disagreement; to date only the mean has been tested. The spread is the stronger test: matching a population mean requires only reproducing population-level regularities, whereas matching disagreement requires representing where the population itself divides.

> Note: Argyle et al. (2023) citation in gap three is flagged for replacement with a better local-LLM reference (per original margin note). Green et al. (2025) was dropped from gap one to keep the convenience-sample point clean; restore as a footnote if coverage is wanted.

### 4.6 Present study and the three questions

**DRAFT.**

> We address these gaps with a benchmark of four LLMs spanning size and provider (gpt-4o-mini, gpt-4.1, claude-haiku-4-5, and gpt-5.4-nano) against GloVe-300 and BGE-large-en-v1.5 ridge models, on a stratified 1,638-word holdout spanning concreteness, frequency, and polysemy, across 18 dimensions. The benchmark is organized around three questions.
>
> The **representation question** asks which method better approximates each human norm, and what the pattern reveals about the two sources of lexical knowledge. Because elicitation can voice only what is usage-encoded while embedding regression can also exploit weak statistical traces of experience-encoded constructs, we expect neither to dominate everywhere: elicitation should be strongest on evaluative and experience-linked dimensions whose content is carried in how words are used, and embedding regression should hold the advantage where meaning rests on bodily experience that text encodes only weakly. At the word level, both methods should do best on words that are frequent, low in polysemy, and concrete, and the embedding advantage should widen as words grow abstract. The embedding model serves as the control that isolates the access mode.
>
> The **extraction question** asks which readout best recovers the human norm. Because a norm is a mean over raters, the expected-value readout is the like-for-like comparison and should align more closely than the top-token readout, which is a point estimate. The expected-value readout requires token log-probabilities, so we also test whether the sampled readout, which needs only repeated draws, recovers the same quantity and extends the method to models that withhold log-probabilities.
>
> The **spread question** asks whether the response distribution carries information beyond its mean. If the distribution behaves like a sample of raters, its spread should covary with human inter-rater disagreement, and most clearly on dimensions whose constructs are usage-encoded. We test this on the logprob-capable models and ask whether the sampled readout recovers the same spread.

---

## 5. Method

### 5.1 Reading the query three ways

**DRAFT.**

> Each word was presented as a single-turn prompt: the dimension-specific instructions, then the word on a new line. The model's response to that one query was read in three ways. The top-token readout records the single most probable scale token, the point estimate from greedy decoding at temperature 0. The expected-value readout takes the probability-weighted mean of the model's distribution over scale tokens, recovering rating information that falls between adjacent integers. The sampled readout draws *k* integer ratings at temperature 1.0 and averages them.
>
> The three differ in what they require and what they recover. The top-token readout needs only a single greedy call and runs on every model, but is locked to integer resolution. The expected-value readout reads the model's output distribution directly and resolves sub-integer values, but needs an API that exposes token log-probabilities. The sampled readout reaches the same sub-integer resolution through repeated draws rather than log-probabilities, so it applies to any model at the cost of additional calls.
>
> The top-token readout is a point estimate that discards the rest of the distribution. The other two are not rival quantities: both recover the model's probability-weighted mean, one analytically from the reported probabilities and one by averaging draws from that same distribution. We expect them to converge, and Part B tests whether they do. A human aggregate norm is itself a mean over raters, so this probability-weighted mean is the like-for-like target and the top-token integer is the coarsened one.
>
> Token log-probabilities are exposed by only the two OpenAI models in this set (gpt-4o-mini, gpt-4.1); we call these the logprob-capable models throughout. The expected-value readout is computed for those two, the top-token readout is collected for all four and anchors the cross-model benchmark, and the sampled readout is run on gpt-4o-mini to test whether the probability-weighted mean survives without log-probability access.

### 5.2 Readout mechanics

**DRAFT.**

> **Top-token readout.** The most probable token was recorded at temperature 0 with `max_tokens` = 1 and parsed as an integer. This readout was collected for all four models and anchors the cross-model benchmark in Part A.
>
> **Expected-value readout.** For the two logprob-capable models, the same query was issued with `logprobs = TRUE` and `top_logprobs = 20`. For each word and dimension, every valid scale token in the top-20 outputs was converted from log-probability to probability. Their summed mass gives coverage, the share of probability on valid tokens; the probabilities were then renormalized over those tokens and the readout computed as the probability-weighted sum of token value times probability. Coverage was 1.000 across all 18 dimensions. Age of acquisition uses a 1–25 scale, but the probability mass concentrated within the recoverable token range, so coverage stayed complete in practice.
>
> A concreteness query, for instance, might return scale-token log-probabilities of -0.11 for "5", -2.41 for "4", and -4.10 for "3", with negligible mass elsewhere. After exponentiation and renormalization this is roughly *p*(5) = .89, *p*(4) = .09, *p*(3) = .02: a top-token readout of 5, an expected-value readout of 4.87, and a distribution spread of 0.34. For a more ambiguous item the mass spreads across adjacent tokens, and the expected value and spread retain information the top token discards.
>
> **Sampled readout.** To test whether the probability-weighted mean survives without log-probability access, every holdout word and dimension was re-elicited on gpt-4o-mini using the identical prompts. We drew *k* = 20 integer ratings per item at temperature 1.0, issued as a single request with the OpenAI `n` parameter so the prompt is processed once and 20 completions returned, parsed each to an integer, and averaged them. Because the reported log-probabilities describe the model's native output distribution, temperature-1.0 draws come from that same distribution, so the sampled and expected-value readouts approximate one quantity by different means. All 20 draws were retained per item, and valid-token coverage was 1.000. Occasional draws that failed integer parsing were dropped, with the item mean taken over the remainder.

> Note: the "distribution spread of 0.34" clause forward-refers to the Part B spread question. Keep if you want spread planted early; cut for a mean-only example otherwise. The Martínez/Argyle positioning sentence is *not* a mechanic — move to the extraction-question gap in the intro or drop; replace Argyle with the local-LLM reference.

### 5.3 Statistical approach

**DRAFT** (restructured to mirror the three questions).

> **Shared conventions.** All analyses used the holdout set only (*n* = 1,638). Methods were compared at the dimension level: each of the 18 dimensions contributes one holdout Pearson *r*, and these 18 values are the unit of analysis, rather than word-level correlations within a dimension. Several analyses model prediction error rather than correlation. For these, each word-by-dimension absolute error was divided by its dimension's response-scale span, so that dimensions measured on different scales contribute comparably; we call this the range-normalized error. Mixed-effects models were fit in lme4 (Bates et al., 2015) with REML, with crossed random effects for word and dimension unless noted; likelihood-ratio tests used ML estimation. Ridge models were fit by 5-fold cross-validation on the 4,915 training words only (Friedman et al., 2010) and evaluated on the holdout.
>
> **The representation question.** The cross-model benchmark compares the top-token readout from the four LLMs against the GloVe-300 and BGE-large ridge models, dimension by dimension (Table 4). The overall accuracy ordering was tested in a mixed-effects model on range-normalized error with method as a fixed factor. To test whether the per-dimension pattern is robust, each featured gpt-4.1-versus-BGE difference was assessed with a paired bootstrap over words (2,000 resamples, fixed seed), scored on the same resampled items, yielding a 95% confidence interval on the advantage at each dimension. Word-level moderators were then added: an extended model interacted method with concreteness, frequency, log synset count, and grammatical versatility jointly, so each interaction is estimated controlling for the others. This moderator model compared the gpt-4.1 top-token readout, the strongest LLM in the benchmark, against BGE-large.
>
> Two analyses identify the failure mode behind the elicitation gaps. First, the reliability of each published mean norm was estimated as true-score variance divided by observed between-item variance, treating each item's measurement-error variance as its reported SD² divided by its rater count; for the Lancaster norms, which report no per-item count, the count was taken as `N_known` for the relevant modality set. We then tested whether the per-dimension embedding advantage tracks reliability, as the noise mode would predict. Second, the dynamic range of each method's predictions was compared to the human norm's range, to characterize how the two representations miscalibrate the scale of their output independently of rank correlation.
>
> **The extraction question.** The expected-value readout was compared to the top-token readout with a paired *t*-test on Fisher-*z* transformed correlations across the 18 dimensions, with a supplementary sign test. The comparison was then pooled across the two logprob-capable models in a single mixed-effects model on range-normalized error, with readout as the contrast. To test whether the expected-value gain is lexically selective or uniform, a separate model interacted readout with concreteness and frequency across the two logprob-capable models. The sampled readout was validated against the expected-value readout on gpt-4o-mini: the mean of 20 temperature-1.0 draws was correlated with both the expected-value readout and the human norms across the full holdout, and a *k*-of-20 resampling analysis (200 resamples at each *k*) characterized how agreement converges as draws accumulate.
>
> **The spread question.** The association between the model's response-distribution spread and human inter-rater disagreement was tested with a mixed-effects model predicting inter-rater SD from logprob SD, with a random slope by dimension, first on gpt-4o-mini and then pooled across both logprob-capable models with model added as a random intercept. Both spreads were standardized within dimension before analysis, since the dimensions differ in scale. Per-dimension correlations index where the association holds. Recovery of the spread without log-probabilities was tested by correlating the SD of the 20 sampled draws with the logprob SD across the 18 dimensions. Published inter-rater SDs were an inclusion criterion for the study, so this analysis covers all 18 dimensions. Word prevalence (Zipf scale) was the frequency covariate throughout.

> OPEN: dimension-level-as-unit choice (18 dimensions as the unit of analysis) still needs either a precedent citation or a one-line defense (e.g., the comparison is between methods *across constructs*, so the construct is the replicate).

---

## 6. Open items (consolidated)

1. **Early intro ordering** (§4.1) — Option A vs. B not chosen. The methods-knowledge-methods bounce is the issue to resolve.
2. **Dimension-level unit** (§5.3) — needs citation or one-line justification.
3. **"What is established" findings** (§4.4) — list actual findings, not just assertions.
4. **Argyle (2023) citation** (§4.5 gap 3, §5.2) — replace with local-LLM reference.
5. **Green et al. (2025)** (§4.5 gap 1) — decide footnote vs. omit.
6. **All of Results prose** — not yet rewritten; still carries flagged sentences. Part A needs the failure-mode frame threaded through (reliability = rules out noise; divergence = rules out difficulty; decision table). Largest remaining lift.
7. **Method cleanups** — execute the socialness drop, n_pos demotion, lexis de-emphasis in the actual Norm Dimensions / Vocabulary subsections (decisions made; text not yet edited).
8. **Spread example clause** (§5.2) — keep or cut the forward-reference.
