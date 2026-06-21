# Beyond the Top Token: Reading LLM Response Distributions in Psycholinguistic Norming

**Jay Timm**
*[Institution]*

---

## Abstract (draft)

Large language models are increasingly used as simulated participants in psycholinguistic norming tasks, but common practice reduces a model's response to a single top-token rating — a point estimate that discards the rest of the response distribution. Human norms are themselves summaries of response distributions: a published norm is a mean over raters, paired with a standard deviation that captures where raters disagree. We argue that LLM norming should be distributional. Using a stratified 1,638-word holdout across 18 psycholinguistic dimensions and four LLMs, we show that the probability-weighted expected value over scale tokens tracks human norms more closely than the top token on every dimension tested — a measurement refinement rather than a new representation. For models that withhold log-probabilities, the same quantity is recoverable by averaging a small number of temperature-1.0 samples (*r* = .997 against the analytic expected value by 20 draws). The distribution's spread carries additional signal: logprob SD tracked human inter-rater disagreement selectively, most strongly on dimensions where disagreement reflects semantic indeterminacy rather than scale-floor compression. Both quantities — mean and spread — carry signal on the same text-accessible dimensions and both are recoverable without log-probability access. The practical implications are for measurement practice: for any LLM-based norming study, the probability-weighted mean is the structurally correct readout, and the spread is a candidate SD that may be usable where calibrated.

---

## 1. Introduction

### 1.1 LLMs as norming participants — the current practice

Psycholinguistic norms are widely used to study word representation and processing, but hand-collecting them for large vocabularies is slow and expensive. Large language models have emerged as a fast, scalable alternative: prompted to rate words on standard psycholinguistic scales, they produce ratings that often correlate substantially with human norms (Trott, 2024; Kello et al., 2025; Hagihara & Miyazawa, 2026; Peng et al., 2025; Conde et al., 2025). The practice has spread quickly.

Standard procedure, however, reduces the model's response to a single decoded number: the top token at temperature 0, the single most probable scale response. This is a point estimate. It treats the model as a single simulated speaker producing one answer.

Human norms are not one answer. They are summaries of response distributions. A published norm reports a mean over many raters and typically a standard deviation capturing how much raters varied. The mean is useful for downstream applications; the SD is informative about ambiguity, idiosyncrasy, and items that warrant caution. Neither is a single speaker's verdict.

The mismatch between how human norms are constructed and how LLM responses are recorded is the starting point for this paper.

### 1.2 The measurement mismatch

When a model assigns log-probability mass across scale tokens for a given word, it is not producing one rating — it is producing a distribution over possible ratings. That distribution has a mean and a spread. The mean is the probability-weighted average of the scale values; the spread is how that probability mass is dispersed.

The top-token readout takes only the mode of this distribution. It is not the mean. For a distribution concentrated on one scale value, mode and mean are close; for a distribution spread across adjacent values, they can differ appreciably, and the mode discards the information in that spread.

A human aggregate norm is the mean of a rater sample. The structurally matched object on the LLM side is the probability-weighted mean of its scale-token distribution — not the mode. This is the core measurement argument.

Two consequences follow. First, the expected value should track human norms more closely than the top token, for the same reason that any unbiased estimator of a mean outperforms a point estimate of the mode. Second, if the model's distribution behaves like a simulated rater sample, its spread should covary with human inter-rater disagreement — recovering the SD-level information that human norms report but LLM norming currently ignores.

### 1.3 What this paper addresses

Three questions organize the paper.

The first is whether the expected-value readout tracks human norms more closely than the top-token readout, and whether the improvement is uniform or concentrated in particular lexical regions. We expect the improvement to be uniform: it is a measurement correction that applies wherever the model's distribution is spread across adjacent tokens, which should not depend systematically on concreteness or frequency.

The second is whether the expected value can be recovered without log-probability access. Several current model families — the GPT-5 series and all Claude models — withhold log-probabilities, so a method requiring them cannot be applied to the most widely used models. We test whether averaging temperature-1.0 samples provides a portable Monte-Carlo approximation to the same quantity.

The third is whether the model's response-distribution spread tracks human inter-rater disagreement. If the distribution behaves like a simulated rater sample, its spread should covary with human SD on the dimensions where disagreement is semantically meaningful. This is a harder test than mean calibration: matching a mean requires only reproducing central tendencies; matching a spread requires recovering where the human population itself divides.

### 1.4 Relationship to representation

This paper asks a measurement question: given an LLM elicitation query, how should the response distribution be summarized? It is distinct from a representation question: which computational route best approximates which human norm?

The two questions are related but not identical. Distributional readouts improve how an elicitation estimate is extracted; they do not change what information the model has access to. An expected value is a better summary of an elicitation response than a top token; it is still an elicitation response. Where the model lacks access to the relevant construct — where elicitation fails because the construct is present in text but not expressible as a rating — a better readout will not repair the underlying failure. This boundary is important: distributional readouts are a measurement improvement, not a representation guarantee.

---

## 2. Background

### 2.1 LLMs as simulated norming participants

A growing literature tests whether LLM ratings correlate with human norms across psycholinguistic dimensions. Trott (2024) evaluated GPT-4 across concreteness, valence, arousal, and dominance as well as Lancaster sensorimotor dimensions, finding strong agreement on affective dimensions and weaker agreement on sensorimotor ones. Kello et al. (2025) extended this to a broader dimension set. Hagihara and Miyazawa (2026) tested early-acquired CDI words and found substantial but construct-dependent alignment. Conde et al. (2025) and Martínez et al. (2025) have begun working with the distributional readout more explicitly.

The standard extraction rule across most of this work is the top token: the model is queried at temperature 0 with `max_tokens = 1` and the response parsed as an integer. This is operationally simple and requires no log-probability access.

### 2.2 Prior distributional readout work

Some recent work has moved toward probability-weighted readouts. Martínez et al. (2025) compared integer and probability-weighted ratings for multi-word expressions and found that the weighted version improved agreement with human norms. Conde et al. (2025) applied a similar approach across a broader set of dimensions. Argyle et al. (2023) used token probabilities as continuous evidence in a political-opinion context.

These results are encouraging but limited. Most evidence comes from a small number of large proprietary models. The question of whether log-probability access can be bypassed by sampling has not been answered for psycholinguistic norming. And the spread of the distribution — the SD-level analogue — has not been tested against human inter-rater disagreement in a systematic way.

### 2.3 What human inter-rater SD represents

The norming literature's treatment of inter-rater SD has evolved substantially. The early tradition treated it as a reliability diagnostic — a marker of unstable items to be screened. That consensus has fractured.

Brainerd and colleagues reframed valence SD as *valence ambiguity*, a psychological property that predicts memory and recognition independently of mean intensity (Brainerd, 2018). Pollock (2018) showed that many abstract words have SDs near the theoretical maximum because raters split between sense-dependent interpretations, indexing lexical non-determinacy rather than measurement failure. For age of acquisition, Birchenough et al. (2017) established that means and SDs are positively correlated, with later-acquired words attracting more structurally meaningful disagreement. Muraki et al. (2025) showed that some sensorimotor rating variance relates to rater imagery vividness rather than word properties.

The semantic diversity tradition (Hoffman et al., 2013) treats variability in contextual usage as a word property rather than a rater artifact.

If human inter-rater SD is at least partly a semantic signal, then a model's response-distribution spread is a candidate for recovering it. A text-trained model has no idiolect and no individual embodiment, so its spread cannot reflect personal response styles; it would have to reflect whatever semantic indeterminacy is encoded in the training text.

---

## 3. Method

### 3.1 Overview

The data for this paper come from the same benchmark as the companion representation paper (Timm, in prep). We summarize the relevant details here.

### 3.2 Materials

**Norm dimensions.** Eighteen psycholinguistic dimensions were included. Eleven sensorimotor dimensions from the Lancaster Sensorimotor Norms (Lynott et al., 2020): auditory, visual, haptic, interoceptive, gustatory, olfactory, foot/leg, hand/arm, head, mouth, and torso (scale 0–5). Three affective dimensions from Warriner et al. (2013): valence, arousal, and dominance (scale 1–9). Four lexical dimensions: concreteness (Brysbaert et al., 2014; scale 1–5), socialness (Diveica et al., 2023; scale 1–7), iconicity (Winter et al., 2023; scale 1–7), and age of acquisition (Kuperman et al., 2012; scale 1–25).

**Vocabulary sample.** Words were drawn from the `lexis` R package (Timm, 2025), a unified resource aligning multiple English psycholinguistic norm datasets. The eligible pool was restricted to words with complete ratings across all dimensions, yielding 6,553 words. A stratified sample was drawn across concreteness quintile × Zipf frequency tertile × polysemy band, with a hybrid floor-plus-proportional allocation strategy. The holdout set of 1,638 words (25%) was assigned within strata. Holdout properties: concreteness *M* = 3.13, *SD* = 1.00; Zipf frequency *M* = 3.49, *SD* = 0.67; WordNet synset count *M* = 3.8, *SD* = 4.2.

### 3.3 LLM procedure

Four LLMs were queried: gpt-4o-mini and gpt-4.1 (OpenAI), claude-haiku-4-5 (Anthropic), and gpt-5.4-nano (OpenAI). Each word was presented as a single-turn prompt with dimension-specific rating instructions followed by the word on a new line, queried at temperature 0 with `max_tokens = 1`.

**Top-token readout.** The most probable scale token was recorded at temperature 0 and parsed as an integer. This readout is available for all four models.

**Expected-value readout.** For the two logprob-capable models (gpt-4o-mini, gpt-4.1), the same API call was made with `logprobs = TRUE` and `top_logprobs = 20`. For each word × dimension, valid scale tokens in the top-20 outputs were converted from log probability to probability, summed to compute coverage (the proportion of mass on valid tokens), and renormalized. The expected value was computed as the probability-weighted sum of (rating × probability) over valid tokens. Coverage was 1.000 across all 18 dimensions. Age of acquisition uses a 1–25 scale; the model's probability mass concentrated within the recoverable token range, so effective coverage remained complete.

For illustration: a concreteness prompt might return top scale-token log probabilities `"5"` = −0.11, `"4"` = −2.41, `"3"` = −4.10. After exponentiation and renormalization, this is approximately *p*(5) = .89, *p*(4) = .09, *p*(3) = .02, yielding a top-token readout of 5, an expected-value readout of 4.87, and a distributional SD of 0.34. For a more ambiguous item, probability mass spreads across adjacent tokens; the expected value and SD retain information the top token discards.

**Sampled readout.** To test whether the expected value can be recovered without log-probability access, every holdout word × dimension was re-elicited on gpt-4o-mini using the identical prompts. We drew *k* = 20 integer ratings at temperature 1.0 (issued as a single request with the OpenAI `n` parameter, so the prompt is processed once and 20 completions returned), parsed each to an integer, and took their mean as a Monte-Carlo estimate of the expected rating. Because the log-probabilities describe the model's native output distribution, temperature-1.0 draws come from that same distribution; the sampled mean and the expected-value readout are two estimators of one quantity. All 20 draws were retained; valid-token coverage was 1.000.

**Distributional spread.** For the logprob-capable models, the SD of the renormalized scale-token distribution — computed as the probability-weighted standard deviation over valid tokens — was taken as the model's spread for each word × dimension. For the sampling route, the SD of the 20 temperature-1.0 draws served as the spread estimate.

Human inter-rater SD — the published per-item standard deviation from each source norm — was retained as the comparison target for the spread analyses.

### 3.4 Statistical approach

All analyses used the holdout set only (*n* = 1,638).

**Expected-value vs. top-token.** The expected-value readout was compared to the top-token readout for the two logprob-capable models separately, using a paired *t*-test on Fisher-*z* transformed dimension-level holdout *r* values (18 dimensions). A supplementary sign test and a pooled mixed-effects model on range-normalized absolute error (each absolute error divided by its dimension's response-scale span, with crossed random effects for word and dimension) confirmed the direction. To test whether the improvement is lexically uniform, an extraction × concreteness and extraction × frequency interaction model was fit over the two logprob-capable models.

**Sampling equivalence.** The sampled readout from gpt-4o-mini was correlated with the expected-value readout across the full 29,484 holdout word × dimension cells. A *k*-of-20 resampling analysis (200 resamples at each *k* from 1 to 20) characterized how the sampled–expected agreement and the sampled–human agreement converge as draws accumulate.

**Spread calibration.** The association between the model's distributional SD and human inter-rater SD was tested in a mixed-effects model with human SD as the outcome, model SD as the fixed predictor, a random slope by dimension, and — for the pooled analysis — model as an additional random intercept. Both spreads were standardized within dimension before analysis (dimensions differ in scale). Per-dimension Pearson *r* between model SD and human SD characterizes where the association holds. Sampling-route spread recovery was tested by correlating the SD of 20 sampled draws with the logprob SD across the 18 dimensions.

---

## 4. Results

### 4.1 Expected value vs. top token

The expected-value readout outperformed the top-token readout on all 18 dimensions for both logprob-capable models.

For gpt-4.1, the expected value improved mean holdout *r* from .636 to .646 (Δ*r* = .010; *t*(17) = 10.4, *p* < .001). For gpt-4o-mini, the improvement was larger: from .605 to .628 (Δ*r* = .023; *t*(17) = 9.99, *p* < .001), consistent with the stronger model's top token already capturing more of the distribution. Pooling both logprob models in a single mixed model on range-normalized error confirmed the extraction effect: the expected value lowered absolute error relative to the top token, β = −0.026, 95% CI [−0.035, −0.018], *t* = −6.0, *p* < .001.

The improvement was lexically uniform. In an extraction × covariate model, EV × concreteness was β = −0.0004, 95% CI [−0.0019, 0.0011], and EV × frequency was β = −0.0001, 95% CI [−0.0018, 0.0015]. The expected-value gain is a measurement refinement inside the LLM pipeline, not a new representation that selectively repairs abstract or rare words.

**Why the gain is small but reliable.** Human aggregate norms occupy sub-integer space; a top token is locked to integer resolution. The expected value recovers the sub-integer information without changing the underlying judgment. A resolution check confirmed this: the human norm correlated .957 with its own rounded value, and rounding the LLM expected value back to the nearest integer erased most of the EV-over-integer gain. The expected value is not a claim that decimals are intrinsically better; it is the structurally matched object for a continuous human mean.

Crediting the OpenAI models with their expected values improves their scores but does not erase the broader embedding advantage over elicitation documented in the companion paper. The gain is a measurement refinement inside the elicitation pipeline.

### 4.2 Sampling recovers the expected value

The sampled expected value (mean of 20 temperature-1.0 draws on gpt-4o-mini) was nearly identical to the logprob-weighted expected value from the same model across the full holdout (*r* = .997; per dimension, *r* ranged from .970 for iconicity to .998 for valence). The two estimators agreed with human norms to the same degree: sampled *r* with human norms = .902, logprob *r* = .901.

**Table 1.** Agreement of the sampled expected value with the logprob expected value and with human norms, as a function of the number of draws *k* (full holdout, *n* = 29,484 word × dimension cells; 200 random *k*-of-20 resamples).

| Draws (*k*) | *r* with logprob expected | *r* with human norms |
| --- | --- | --- |
| 1  | .977 | .884 |
| 2  | .988 | .893 |
| 3  | .991 | .897 |
| 5  | .994 | .899 |
| 10 | .996 | .901 |
| 15 | .997 | .902 |
| 20 | .997 | .902 |

*Note.* Reference ceiling: logprob expected value vs. human norms *r* = .901. Token coverage was 1.000 for the sampling run; occasional draws that failed integer parsing were dropped, with the item mean computed over remaining draws.

Agreement with the logprob expected value was high even at *k* = 1 (*r* = .977), was essentially asymptoted by *k* = 5 (*r* = .994), and reached the practical ceiling of *r* = .997 by *k* = 10–15. The sampled mean therefore approximates the analytic expected value reliably with a modest number of draws.

### 4.3 Distributional spread tracks human inter-rater disagreement

The model's response-distribution spread (logprob SD) varied substantially across dimensions: dimension means ranged from *M* = 0.10 for gustatory to *M* = 1.49 for age of acquisition (the latter reflecting its wider 1–25 scale). Both logprob SD and human inter-rater SD were standardized within dimension before analysis.

On gpt-4o-mini, a mixed-effects model (human inter-rater SD ~ logprob SD, random slope by dimension) showed a reliable positive main effect of logprob SD (β = 0.132, *p* = .002). The effect strengthened when pooled across both logprob-capable models with model as an additional random intercept: β = 0.181, 95% CI [0.119, 0.243], *t* = 5.7, *p* < .001.

The association was selective. Per-dimension correlations were positive on semantically rich dimensions — concreteness (*r* = .413), age of acquisition (.401), gustatory (.398), olfactory (.247), visual (.222) — and near zero or negative on floor-heavy Lancaster body-part norms: hand/arm (*r* = .020), torso (−.090), auditory (−.097).

Logprob spread therefore tracks human disagreement where disagreement reflects semantic indeterminacy, and loses calibration where human scale spread is bounded near floor or reflects embodied individual differences not encoded in text.

### 4.4 Sampling recovers the spread

The SD of 20 sampled draws from gpt-4o-mini recovered the logprob SD across the 18 dimensions (*r* = .927 overall; per-dimension correlations from .64 for iconicity to .91 for gustatory). The sampling route does not merely approximate the distribution's mean; it also recovers its spread.

For models that expose log-probabilities, mean and spread are available analytically from a single call. For models that do not, both are recoverable by averaging and summarizing a modest number of sampled ratings.

---

## 5. Discussion

### 5.1 The measurement argument

A published norm is a mean over raters. A top-token LLM rating is the model's single most probable answer — the verdict of one simulated speaker at temperature 0. Reading the expected value is the move from that speaker to the community-level quantity a norm actually is.

That move paid consistently: the expected value outperformed the top token on all 18 dimensions for both logprob-capable models. The improvement was small (Δ*r* = .010–.023) and lexically uniform — it is a resolution correction, not a representation fix. But it is reliable and it carries across dimensions and models. For a field that routinely compares methods by their *r* with human norms, uniform improvements of this size are not negligible at the margin.

The community-versus-speaker framing gives the improvement a theoretical grounding. Ratings are sourced in individual linguistic experience, but a norm aggregates over a community that no individual member embodies (Timm, 2016). The top token is the mode; the expected value is the mean. The human norm is a mean. The match is not just structural; it follows from what the measurement is.

### 5.2 Sampling as a portable readout

The near-identity of the sampled and analytic expected values (*r* = .997) shows that the distributional readout is not gated on API features. Five to ten draws is sufficient for practical purposes; twenty draws adds little over fifteen. The implication is direct: for GPT-5, Claude, Gemini, and any other model family that withholds log-probabilities, the distributional readout is still available by sampling at temperature 1.0 and averaging a handful of responses.

This also means the finding from Martínez et al. (2025) and Conde et al. (2025) — that probability-weighted readouts improve norm alignment — is not confined to OpenAI models. It generalizes wherever the model's output distribution at temperature 1.0 is a faithful reflection of its native distribution.

The practical ceiling matters. Sampling converges quickly: *k* = 1 already achieves *r* = .977 against the analytic expected value. Diminishing returns set in fast. For a large vocabulary run where per-word cost matters, even two or three draws per item may be sufficient.

### 5.3 What the spread recovers and why

The finding that logprob SD tracks human inter-rater SD (pooled β = 0.181, *p* < .001) joins a debate about what inter-rater SD represents.

The early treatment of inter-rater SD as a reliability diagnostic underweights what subsequent work has shown: that disagreement often reflects semantic properties of the word rather than rater noise. Concreteness SD tracks sense-dependent ambiguity (Pollock, 2018). AoA SD reflects acquisition-history variability. Gustatory and olfactory SD reflects evaluative-hedonic indeterminacy for words in domains where the human rating task engages very different sense-use frequencies across raters.

The selectivity of the calibration is itself informative. Logprob SD correlates with human SD on the dimensions where human disagreement is a semantic signal, and fails to calibrate on floor-heavy body-part dimensions where human SD is bounded near zero by the rating task's structure. A text-trained model has no idiolect, so its spread cannot track variance grounded in individual motor imagery or embodied response styles (Muraki et al., 2025). The model's uncertainty is lexical-semantic, not experiential.

This gives the logprob SD a specific interpretive status. It is not the same as human inter-rater SD: it lacks the individual-difference component. It is not semantic diversity in Hoffman et al.'s (2013) sense: it comes from a prompted rating task, not from corpus usage distributions. It sits between the two — the model's posterior over scale values, encoding whatever semantic indeterminacy is stabilized in its training text.

### 5.4 The Trott (2024b) question

Trott (2024b) asks whether temperature or prompting can induce stochasticity in LLM responses that resembles human variance, or whether the models are simply ill-suited to individual differences. Li, Li, and Qiu (2025) return a pessimistic answer for opinion surveys, where silicon samples systematically under-disperse relative to human respondents.

The present results return a conditional answer. The model's spread is calibrated to human disagreement on semantically rich dimensions and homogenized elsewhere. The condition is the same construct boundary that organizes the representation benchmark: where meaning circulates in text as evaluation or shared experience, the model's distribution tracks human distribution in both mean and spread. Where meaning rests on direct experience or bodily involvement, calibration fails for mean and spread alike.

The second channel is not independent of the first: the dimensions where the LLM's mean best matches human norms are the same ones where its logprob SD best matches human disagreement. Mean and spread carry the same representational signature.

### 5.5 Practical recommendations

**Report the readout.** Any LLM norming paper should specify whether ratings are top-token, expected-value, or sampled. These are not equivalent; the top-token readout systematically underestimates the human norm alignment of the model.

**Use the expected value when logprobs are available.** It adds no cost (one API call, same parameters plus `logprobs = TRUE`), improves alignment on every dimension, and provides a coverage statistic that serves as a data-quality flag.

**Use sampled means when logprobs are unavailable.** Five to ten draws at temperature 1.0 suffice for most practical purposes. Report the number of draws, parsing rules, and invalid-response rate.

**Report coverage.** Valid-token coverage was 1.000 in this study. For narrow scales, unusual models, or scales with non-integer tokens, coverage can fail; a coverage flag below ~.99 warrants caution.

**Treat spread as a candidate SD.** On semantically rich dimensions, logprob SD or sampled SD can serve as a machine-generated inter-rater SD with a known interpretation: it captures semantic indeterminacy, not rater-specific variation. Validate against human SD before reporting as an analogue.

**Separate readout from representation.** Better readout improves measurement resolution. It does not guarantee that the model has access to the target construct. Where elicitation fails — on sensorimotor or other experience-grounded dimensions — a better readout will improve the extraction of whatever signal is present but will not supply signal that is absent.

### 5.6 Limitations

The expected-value and logprob-SD analyses rest on two logprob-capable models from one provider (OpenAI). The sampling equivalence (*r* = .997) shows the same quantity is recoverable elsewhere, but the analytic expected value was validated only within the OpenAI family. Future work should test the logprob-based results on other providers when log-probability access is available.

The AoA scale (1–25) returns only 20 tokens from `top_logprobs`; in practice mean coverage was .999, but this should be verified for other long-scale dimensions or models with different tokenization.

The spread calibration result is selective, and the mechanism on the calibrating dimensions is interpretive rather than directly tested. The claim that logprob SD tracks semantic indeterminacy (rather than, say, model calibration noise) follows from the selectivity pattern but has not been verified against independent measures of semantic ambiguity per dimension. This is a productive direction for follow-up.

---

## 6. Conclusion

LLM norming has moved quickly to using model responses as norm-like measurements without settling what the right measurement is. The answer is not the top token. A human norm is a mean over raters; the structurally matched LLM readout is the probability-weighted mean of the model's scale-token distribution. That readout consistently improves alignment with human norms, is recoverable by sampling for any model family, and yields a spread estimate that tracks human inter-rater disagreement on semantically rich dimensions. The practical rule: treat the LLM as a probabilistic instrument and read its distribution, not its mode.

---

## Data and Code Availability

The vocabulary sample, norm datasets, LLM runs, and standardized prompt instructions are available in the `lexis` R package (Timm, 2025; https://github.com/jaytimm/lexis). The distributional analyses are reproduced in `analysis/paper1/analysis-paper1-multimodel.Rmd`. Integer ratings and logprob distributions for all four models were collected on 2026-06-01/02 under the run IDs documented in the companion paper (Timm, in prep). The sampling run on gpt-4o-mini is `gpt-4o-mini_sampled_20260601T192832` (temperature = 1.0, 20 draws per item via the OpenAI `n` parameter, all per-draw integers archived). A citable DOI will be deposited on acceptance.

---

## References

Andrews, M., Vigliocco, G., & Vinson, D. (2009). Integrating experiential and distributional data to learn semantic representations. *Psychological Review, 116*(3), 463–498.

Argyle, L. P., Busby, E. C., Fulda, N., Gubler, J. R., Rytting, C., & Wingate, D. (2023). Out of one, many: Using language models to simulate human samples. *Political Analysis, 31*(3), 337–351.

Bates, D., Mächler, M., Bolker, B., & Walker, S. (2015). Fitting linear mixed-effects models using lme4. *Journal of Statistical Software*, *67*(1), 1–48.

Birchenough, J. M. H., Davies, R., & Connelly, V. (2017). Rated age-of-acquisition norms for over 3,200 German words. *Behavior Research Methods, 49*(2), 484–501.

Brainerd, C. J. (2018). The emotional-ambiguity hypothesis: A large-scale test. *Psychological Science, 29*, 98–115.

Brysbaert, M., Warriner, A. B., & Kuperman, V. (2014). Concreteness ratings for 40 thousand generally known English word lemmas. *Behavior Research Methods*, *46*, 904–911.

Conde, J., González, M., Grandury, M., Martínez, G., Reviriego, P., & Brysbaert, M. (2025). Psycholinguistic word features: A new approach for the evaluation of LLMs alignment with humans. *Proceedings of the 4th Generation, Evaluation and Metrics Workshop*.

Conde, J., Grandury, M., Fu, T., Arriaga, C., Martínez, G., Clark, T., Trott, S., Green, C. G., Reviriego, P., & Brysbaert, M. (2025). Adding LLMs to the psycholinguistic norming toolbox: A practical guide to getting the most out of human ratings. *arXiv*. https://arxiv.org/abs/2509.14405

Diveica, V., Pexman, P. M., & Binney, R. J. (2023). Quantifying social semantics: An inclusive definition of socialness and ratings for 8,388 English words. *Behavior Research Methods*, *55*, 461–473.

Hagihara, H., & Miyazawa, K. (2026). How well do large language models mirror human cognition of word concepts? *Behavior Research Methods, 58*, 58.

Hoffman, P., Lambon Ralph, M. A., & Rogers, T. T. (2013). Semantic diversity: A measure of semantic ambiguity based on variability in the contextual usage of words. *Behavior Research Methods, 45*, 718–730.

Kello, C. T., Bruna, P., & Thao, K. (2025). Contextual assembly of lexical functions in large language models. *Behavior Research Methods, 58*, 19.

Kuperman, V., Stadthagen-Gonzalez, H., & Brysbaert, M. (2012). Age-of-acquisition ratings for 30,000 English words. *Behavior Research Methods*, *44*, 978–990.

Li, D., Li, L., & Qiu, H. S. (2025). ChatGPT is not a man but Das Man: Representativeness and structural consistency of silicon samples generated by large language models. *arXiv*. https://arxiv.org/abs/2507.02919

Lynott, D., Connell, L., Brysbaert, M., Brand, J., & Carney, J. (2020). The Lancaster Sensorimotor Norms. *Behavior Research Methods*, *52*, 1271–1291.

Martínez, G., Molero, J. D., González, S., Conde, J., Brysbaert, M., & Reviriego, P. (2025). Using large language models to estimate features of multi-word expressions. *Behavior Research Methods, 57*, 5.

Muraki, E. J., Born, S., & Pexman, P. M. (2025). Grasping variance in word norms: Individual differences in motor imagery and semantic ratings. *Journal of Cognition, 8*(1), 12.

Peng, B., Hsu, Y.-y., Chersoni, E., Qiu, L., & Huang, C.-R. (2025). Multilingual prediction of semantic norms with language models. *Language Resources and Evaluation, 59*, 3911–3937.

Pollock, L. (2018). Statistical and methodological problems with concreteness and other semantic variables. *Behavior Research Methods, 50*, 1198–1216.

Timm, J. (2016). *Lexical variation, lexical innovation, and speaker motivations* [Doctoral dissertation, University of New Mexico].

Timm, J. (2025). *lexis: English psycholinguistic norms in a unified tidy resource* (Version 0.1.0) [R package]. GitHub. https://github.com/jaytimm/lexis

Timm, J. (in prep). Language models and embeddings capture different psycholinguistic constructs: A multi-model benchmark across 18 dimensions.

Trott, S. (2024a). Can large language models help augment English psycholinguistic datasets? *Behavior Research Methods, 56*, 6082–6100.

Trott, S. (2024b). Large language models and the wisdom of small crowds. *Open Mind, 8*, 723–738.

Warriner, A. B., Kuperman, V., & Brysbaert, M. (2013). Norms of valence, arousal, and dominance for 13,915 English lemmas. *Behavior Research Methods*, *45*, 1191–1207.

Winter, B., Perlman, M., & Majid, A. (2023). Iconicity ratings for 14,000+ English words. *Behavior Research Methods*, *55*, 1640–1655.
