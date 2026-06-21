# Possible Separate Article: Distributional Readouts for LLM Norming

## Possible Article Type

This is not currently planned as a Perspective. A Perspective could be one possible
outlet, but that route is fit-sensitive and may require an editor query before
submission. The safer framing is a possible separate article or methods note if
the distribution results cannot be incorporated cleanly into Paper 1.

The reason a separate article might work is that the central contribution is not
just another benchmark. It is an argument about what kind of measurement object
an LLM response is when the model is used as a simulated norming participant.

## Working Titles

- From Single Tokens to Response Distributions: Rethinking LLMs as Norming
  Participants
- LLM Norming Should Be Distributional
- Beyond the Top Token: Reading LLM Response Distributions in Psycholinguistic
  Norming
- Treating LLM Ratings as Distributions, Not Answers

## Core Claim

If LLMs are used as simulated participants in psycholinguistic norming, their
responses should not be reduced to a single top-token rating by default. Human
norms are aggregates over many raters, and LLMs expose or can approximate a
distribution over possible ratings. That distribution has a mean, a spread, and
sampling behavior. These are measurement-relevant quantities.

The field has mostly asked:

> Do LLM ratings correlate with human norms?

This possible article argues that the next question is:

> What should count as the LLM's rating when the model itself represents a
> distribution over possible ratings?

## One-Sentence Thesis

LLM norming should move from deterministic single-token ratings to distributional
readouts, because the probability-weighted mean is the like-for-like analogue of
a human aggregate norm and the distribution's spread can carry information about
where human raters disagree.

## Why This Is Not Paper 1

Paper 1 asks a representation question:

> Which computational route best approximates each human norm, and what does the
> pattern reveal about lexical knowledge?

This possible separate article asks a measurement question:

> Given an LLM elicitation query, how should the resulting response distribution
> be summarized, reported, and interpreted?

The empirical overlap is useful, but the conceptual object is different. Paper 1
is about construct-dependent access to lexical information. A separate article
would be about measurement practice for LLM-as-participant norming.

## Intended Contribution

1. Review the current practice of using LLMs as norming participants.
2. Identify a measurement mismatch: human norms are means over raters, whereas
   common LLM procedures record a single top token.
3. Argue for distributional readouts:
   - expected-value readout when logprobs are available
   - sampled readout when logprobs are unavailable
   - spread as a candidate analogue of human disagreement
4. Provide empirical demonstrations using the existing norming benchmark.
5. State boundary conditions: distributional readouts are only as meaningful as
   the underlying elicitation task; they do not solve representation failures.

## Proposed Structure

### 1. Opening: The Wrong Unit Of Analysis

Start with the contrast between human norming and common LLM elicitation.

Human norming:

- many raters
- distribution of responses
- reported mean, often SD
- disagreement is informative

Common LLM norming:

- one prompt
- one decoded response
- usually top token or parsed integer
- distribution discarded

Core opening move:

> A human norm is not a single answer. It is a summary of a response
> distribution. If LLMs are used as simulated norming participants, the natural
> object of comparison is therefore not the model's top token but its response
> distribution.

### 2. Short Review: LLMs As Norming Participants

Review recent work showing that LLM ratings often correlate with human norms.

Likely points:

- LLMs can approximate affective, lexical, and some semantic norms.
- Performance is uneven across constructs.
- Many studies use deterministic prompts or top-token/integer responses.
- Some recent work has used logprob-weighted or probabilistic readouts, but the
  practice is not yet standard.

Purpose of the review:

Not to litigate whether LLMs "work," but to show that the field has moved quickly
to using LLM responses as norm-like measurements without settling what the
measurement actually is.

### 3. The Measurement Mismatch

Develop the main conceptual critique.

Human norm:

```text
mean over raters = expected value of a human response distribution
```

Top-token LLM readout:

```text
mode or greedy point estimate = most probable rating token
```

These are not like-for-like. A top-token rating discards sub-integer information
and ignores uncertainty between adjacent values.

Example:

- Model assigns most probability to `5`, but also substantial mass to `4`.
- Top token gives `5`.
- Expected value gives something like `4.7`.
- The latter is closer in kind to a human aggregate mean.

Core claim:

> If the target is a human mean, the probability-weighted mean of the model's
> rating distribution is the natural LLM readout.

### 4. Three Readouts

Define the readouts cleanly.

Top-token readout:

- one greedy response
- available for all models
- cheap
- integer or parsed text
- discards distribution

Expected-value readout:

- uses token logprobs
- computes probability-weighted mean over valid scale tokens
- recovers sub-integer rating information
- limited to models/APIs that expose logprobs

Sampled readout:

- draws repeated ratings at temperature 1.0
- averages parsed ratings
- Monte Carlo approximation to the expected value
- available even when logprobs are unavailable
- costs more calls/tokens

Key framing:

Expected-value and sampled readouts are not rival constructs. They are two
estimators of the model's probability-weighted mean.

### 5. Demonstration 1: Expected Value Beats Top Token

Use existing results as an empirical demonstration.

Possible claims:

- Across 18 dimensions, expected-value readout improves alignment with human
  norms relative to top-token readout.
- The improvement is small but reliable.
- It is lexically broad rather than restricted to one narrow region of the
  vocabulary.

What this demonstrates:

The distribution contains useful measurement information that the top token
throws away.

What not to overclaim:

This does not mean elicitation is always the best representation method. It only
means that, conditional on using elicitation, the response distribution should be
read properly.

### 6. Demonstration 2: Sampling Recovers The Expected Value

Problem:

Several current model families do not expose token logprobs, so expected-value
readout may look impractical.

Demonstration:

- Repeated temperature-1.0 samples recover the expected-value readout.
- Five to ten samples may be enough for strong agreement.
- Twenty samples gives very high agreement in the current benchmark.

Conceptual payoff:

Distributional readout does not require privileged access to model internals.
Sampling can approximate the same quantity through repeated draws.

Practical recommendation:

When logprobs are unavailable, collect multiple samples and report the sampled
mean, along with the number of draws and parsing/validity rates.

### 7. Demonstration 3: Spread And Human Disagreement

Human norm sets often report SD because rater disagreement is meaningful.

LLM response distributions also have spread:

- logprob distribution spread
- sampled rating SD
- entropy or adjacent-token uncertainty

Empirical demonstration:

- The LLM distribution's spread covaries with human inter-rater disagreement on
  some dimensions.
- The effect is selective, likely strongest where elicitation is otherwise
  representationally appropriate.

Interpretation:

The spread is not just decoding noise. In favorable cases, it carries information
about where the human population itself is less settled.

Boundary:

Spread calibration should not be assumed globally. If the model is poorly aligned
with the construct mean, its spread may also be uninformative or misleading.

### 8. Boundary Conditions: Distributional Readouts Do Not Fix Access Failures

This is where Paper 1 can be referenced conceptually.

Distributional readouts improve how we summarize an elicitation response, but
they do not solve every elicitation failure.

If a construct is not accessible to elicitation, reading the distribution more
carefully may produce a better summary of the wrong signal.

This boundary is important:

- Expected value improves measurement resolution.
- Sampling improves portability across model APIs.
- Spread may capture disagreement.
- None of these guarantee that the model has access to the target construct.

Possible bridge:

> Distributional readouts are a measurement improvement, not a representation
> guarantee.

### 9. Recommendations For The Field

Possible checklist:

1. Report whether ratings are top-token, expected-value, or sampled readouts.
2. Prefer expected-value readouts when logprobs are available.
3. Use sampled readouts when logprobs are unavailable.
4. Report the number of samples, temperature, parsing rules, and invalid-response
   rate.
5. Report valid-token coverage for logprob methods.
6. Treat response spread as potentially informative but validate it against human
   disagreement where possible.
7. Avoid treating a deterministic LLM rating as equivalent to a human norm mean
   without justification.
8. Evaluate readouts separately from model/construct suitability.

### 10. Closing: LLMs As Measurement Instruments

End with a field-level claim.

LLMs should not be treated as ordinary respondents whose first answer is the
measurement. They are probabilistic instruments whose output distributions can be
read at different levels of resolution.

The goal is not to anthropomorphize the distribution as a literal population of
raters. The goal is to use the statistical structure the model already provides
to make LLM norming more faithful to what human norming actually reports.

Possible closing sentence:

> The next generation of LLM norming should not ask only which model gives the
> right answer, but which part of the model's response distribution is the right
> measurement.

## Figures And Tables

### Figure 1: Human Norming Versus LLM Readouts

Conceptual diagram:

- Human rater distribution -> mean and SD
- LLM token distribution -> top token, expected value, spread
- Sampling -> Monte Carlo approximation

### Figure 2: Top Token Versus Expected Value

Dimension-level paired comparison:

- x-axis: top-token correlation
- y-axis: expected-value correlation
- diagonal line

### Figure 3: Sampling Convergence

Agreement with expected-value readout as number of samples increases:

- k = 1, 2, 5, 10, 20
- mean and uncertainty bands over resamples

### Figure 4: Spread And Human Disagreement

Dimension-level or word-level association:

- human SD vs model distribution spread
- maybe facets by dimension group

### Table 1: Readout Comparison

Columns:

- readout
- requires logprobs?
- requires repeated calls?
- output
- advantages
- limitations

## Relationship To Paper 1

Paper 1 can cite or motivate this separate article if it ever exists, but Paper 1
should not depend on it.

Paper 1:

- Which computational route works for which construct?
- Why do some elicitation failures occur?
- Uses BGE axis probe to diagnose access versus supervision.

Possible separate article:

- Conditional on using LLM elicitation, how should the response be read?
- Uses expected value, sampling, and spread as measurement demonstrations.

Shared principle:

Both papers argue against treating LLM ratings as simple one-shot answers.

Different objects:

- Paper 1: access to lexical information.
- Possible separate article: measurement from probabilistic outputs.

## Possible Abstract Draft

Large language models are increasingly used as simulated participants in
psycholinguistic norming tasks, but common practice often reduces a model's
response to a single decoded rating. This collapses a probabilistic response into
a point estimate, even though human norms are themselves summaries of response
distributions. We argue that LLM norming should be distributional. When token
probabilities are available, the probability-weighted mean over scale tokens is
the natural analogue of a human aggregate norm; when probabilities are withheld,
repeated sampling can approximate the same quantity. The spread of the response
distribution may also carry information about where human raters disagree,
although this should be validated rather than assumed. Reframing LLM responses as
distributions clarifies the distinction between measurement resolution and
construct access: distributional readouts can improve elicitation, but they do
not guarantee that the model represents the target norm. We outline practical
recommendations for reporting and evaluating distributional LLM norming.

