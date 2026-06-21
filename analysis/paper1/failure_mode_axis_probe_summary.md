# Failure-Mode Axis Probe: Working Summary

## Why We Ran This

The main representation result compares LLM elicitation with supervised embedding
regression. That comparison is informative, but it has a possible confound:
embedding regression is trained on norm labels, whereas elicitation is zero-shot.
If BGE ridge beats elicitation, a reviewer can ask whether the advantage reflects
supervised calibration rather than a difference in access to lexical information.

The axis probe was added to test that concern directly. It asks whether the same
dimensions that favor embedding regression are already recoverable from raw BGE
geometry, before fitting a regression model.

The probe has now been run. It does not merely pass or fail as a confound check;
it returned a graded result that decomposes the embedding advantage into
supervision-independent and supervision-dependent cases (see Main Result and
Interpretation). It diagnoses the failure-mode frame and is now promoted to the
second beat of Paper 1; it does not replace the main LLM-vs-ridge benchmark.

The result also settled the Paper 1 / Paper 2 split (see final section): with the
probe serving as the second beat, the distribution work is no longer load-bearing
for Paper 1 and moves to a separate Paper 2.

## Framing Cache

> SCOPE GUARD. The probe beats elicitation on **6 of 18** dimensions, not on the
> whole embedding advantage. Every line below must carry that scope ("on several
> dimensions," "a subset"), or it overclaims exactly the cases the probe did *not*
> recover — the dimensions where ridge beats elicitation but the probe does not.
> Two of the six (mouth, torso) are near-boundary and need the paired bootstrap
> before being counted as clean probe wins; the claim may be 4 of 18, not 6, once
> intervals are in. Do not harden any of these sentences until that is settled.

Core pitch:

> A shared-item benchmark plus an unfitted embedding probe shows that some
> elicitation failures are access failures: the relevant signal is present in
> text geometry but not available through rating-style generation.

Shorter version:

> Some LLM elicitation failures are not absence-of-signal failures. The signal is
> present in text geometry, but inaccessible to rating-style generation.

Representation-paper version:

> LLM elicitation and embedding regression succeed on different psycholinguistic
> constructs, and an unfitted BGE axis probe shows that several elicitation
> failures reflect access rather than supervised calibration.

Cover-letter / venue version:

> The paper moves beyond asking whether LLMs correlate with human norms. It uses
> a shared-item benchmark and an unfitted embedding-geometry probe to diagnose
> why elicitation fails where it fails.

Reviewer-defense version:

> Because the axis probe uses no fitted regression weights, probe-over-elicitation
> cases cannot be explained by supervised training on norm labels.

Abstract candidate (scoped):

> On a subset of dimensions, an unfitted BGE axis probe outperformed zero-shot
> elicitation, showing that the relevant signal was present in embedding geometry
> before any supervised calibration.

## Method

The probe uses `analysis/output/core_vocab_long.csv`, the existing train/holdout
split, and `analysis/output/bge_large_embeddings.csv`.

For each norm dimension:

1. Use training words only.
2. Sort training words by the human norm score.
3. Take the lowest `n` and highest `n` training words as the two scale poles.
4. L2-normalize BGE vectors.
5. Average the low-pole vectors and high-pole vectors.
6. Define a single axis as `high_mean - low_mean`.
7. Project each holdout word onto that axis.
8. Correlate holdout projections with holdout human ratings.

No regression weights are fit. Labels are used only to locate the two ends of the
scale in the training split. The holdout set is never used to construct the axis.

The Rmd runs pole-size sensitivity at `n = 20`, `30`, `50`, and `100`. The
primary comparison is currently `n = 50`, because it stays inside the original
20-50 design window. The `n = 100` run is useful as an additional sensitivity
check: it is stronger on average, but should be treated as exploratory unless we
decide to make pole size a tuning parameter.

Files:

- `analysis/paper1/failure-mode-axis-probe.Rmd`
- `analysis/paper1/failure-mode-axis-probe.html`
- `analysis/output/failure_mode_axis_probe.csv`
- `analysis/output/failure_mode_axis_probe_summary.csv`
- `analysis/output/failure_mode_axis_probe_focus.csv`

## Comparisons

The focused comparison uses three quantities for each dimension:

- `gpt-4.1 top-token`: elicitation comparator
- `bge_large` axis probe: unfitted BGE geometry
- `bge_large` ridge: supervised embedding regression

The ridge model remains the main performance benchmark. The axis probe answers a
different question: whether the relevant signal is visible in embedding geometry
without supervised regression.

## Pole-Size Sensitivity

BGE holdout correlations improve with larger pole size:

| Pole words per side | Mean holdout r | Median holdout r |
| --- | ---: | ---: |
| 20 | .577 | .563 |
| 30 | .598 | .579 |
| 50 | .616 | .607 |
| 100 | .632 | .612 |

The `n = 100` probe is stronger on average than `n = 50`, with the biggest gains
for head, foot/leg, torso, olfactory, visual, and auditory. The qualitative
probe-over-LLM pattern does not change: six dimensions still beat the LLM, and
foot/leg becomes a near tie. For the paper, `n = 50` remains the conservative
primary setting unless we explicitly justify `n = 100` as the preferred pole
size.

## Main Result

The LLM loses to supervised BGE ridge on 13 of 18 dimensions. That is the broad
representation result.

The LLM loses to the unfitted BGE axis probe on 6 of 18 dimensions, with several
near-boundary cases. With `n = 50`, the clearest probe-over-LLM cases are:

| Dimension | BGE axis r | gpt-4.1 top-token r | BGE ridge r |
| --- | ---: | ---: | ---: |
| dominance | .701 | .250 | .742 |
| auditory | .623 | .337 | .750 |
| interoceptive | .701 | .637 | .800 |
| mouth | .616 | .594 | .728 |
| head | .480 | .257 | .628 |
| torso | .566 | .523 | .709 |

The near-boundary cases matter because the axis probe is deliberately weak. It is
a one-dimensional, unfitted projection. It is expected to underperform ridge.
When it still beats elicitation, the result is diagnostic.

> OUTSTANDING: the six probe-over-LLM cases are point estimates. Auditory
> (.623 vs .337), dominance (.701 vs .250), and head (.480 vs .257) are wide and
> safe. Interoceptive (.701 vs .637), mouth (.616 vs .594), and torso
> (.566 vs .523) are close. Run the same 2,000-resample paired bootstrap used
> elsewhere on probe-vs-elicitation for these six, scored on the same resampled
> words. If the close cases do not clear zero, the clean count is 4, not 6 — still
> sufficient, but the manuscript must report whichever the intervals support.
> Confirm the qualitative count is stable across n = 20/30/50/100 as well.

## Interpretation

The axis probe decomposes the embedding advantage.

First, there are dimensions where raw BGE geometry already beats elicitation.
These are the strongest access-failure cases. The signal is present in text
geometry before supervised calibration, so the ridge advantage cannot be dismissed
as "the model saw labels."

Second, there are dimensions where ridge beats elicitation but the axis probe does
not. These may still contain recoverable text signal, but the signal is not cleanly
organized along a simple low-to-high axis. Supervised regression may be needed to
recover weaker, multidimensional traces.

Third, there are dimensions where elicitation beats both the axis probe and ridge.
These remain the LLM-friendly cluster: valence, arousal/AoA-like evaluative or
experience-linked dimensions, and especially gustatory and olfactory in the
current results.

The result strengthens the failure-mode frame. A ridge-only divergence could be
criticized as a supervision effect. A matching axis-probe divergence shows that
some elicitation failures are not absence-of-signal failures. The signal is there;
elicitation is not accessing it effectively.

## How Promotable Is This?

If the distribution and SD analyses are removed from the main paper, the axis
probe becomes highly promotable. It is no longer competing with a separate
measurement story. Instead, it supplies the second beat of the representation
paper: first establish the construct boundary, then diagnose what that boundary
means.

Best role in the revised paper:

1. Main benchmark: LLM elicitation vs BGE ridge. This establishes that the two
   methods succeed on different constructs.
2. Failure-mode frame: embedding success where elicitation fails identifies cases
   where the miss is not simply low norm reliability or impossible recovery from
   text.
3. Axis-probe diagnostic: some of those embedding advantages survive without
   supervised regression, showing that the relevant signal is already present in
   raw BGE geometry.

In that version, the axis probe is not a side robustness check. It is the analysis
that prevents the main benchmark from being read as a simple supervised-vs-zero-shot
comparison. The paper can make a stronger claim because it has three levels of
evidence:

- Elicitation versus ridge shows the empirical boundary.
- Reliability and convergence/divergence give the failure-mode interpretation.
- The unfitted BGE axis probe shows that several elicitation failures are
  recoverable from text geometry before supervised calibration.

The probe still should not become a sprawling third model family. It should be
presented as a diagnostic probe of BGE geometry. A compact table with the six
probe-over-LLM dimensions is probably enough in the main text, with the full
18-dimension and pole-size sensitivity tables left in the companion Rmd or
supplement.


Possible manuscript sentence:

> To test whether the BGE advantage simply reflected supervised training on norm
> labels, we also scored holdout words with an unfitted BGE axis defined by the
> highest- and lowest-rated training words. This probe recovered several
> elicitation failures, showing that the relevant signal was present in embedding
> geometry before supervised calibration.

## What This Buys The Paper

The original claim was:

> LLM elicitation and embedding regression succeed on different constructs.

The axis probe allows a stronger claim:

> Some elicitation failures are access failures: the relevant information is
> recoverable from text geometry, but not from asking the LLM to produce a rating.

That is more theoretically precise. It also makes the paper harder to dismiss as
another benchmark of model correlations.

The axis probe gives the representation paper a cleaner second beat than the
distributional response analysis has so far. It stays inside the same conceptual
problem: which lexical information is available to which computational route?

## Distribution Piece And Separate-Article Thoughts

The distribution results answer a different question from the representation
results.

Representation question:

> Which computational route best approximates each human norm, and what does that
> reveal about lexical knowledge?

Distribution question:

> Given one LLM query, how should the model's output distribution be read?

That second question is good, but it changes the object of the paper. It brings in
top-token versus expected-value readouts, sampling as a Monte Carlo estimator, and
spread as a possible analogue of human disagreement. Those results are valuable,
but they have repeatedly been hard to integrate because they pull the manuscript
from representation/failure diagnosis into measurement mechanics.

If there were a clean separate outlet, the split would be:

- Paper 1: representation comparison, lexical moderators, failure-mode frame,
  BGE axis probe.
- Separate article or methods note: LLM response distributions as norming
  estimates, including expected value, sampling without logprobs, and
  spread-disagreement calibration.

The cost is practical: a separate article may never happen, and a Perspective
format is not something to assume. Some journals explicitly ask authors to query
editors before submitting unsolicited Perspectives, both for fit and possible
overlap with submissions already under review. So Paper 1 should not be shaped
around an imagined Paper 2 rescue.

A compromise is to keep the distribution work, but demote it:

- Keep expected-value versus top-token if it improves elicitation estimates and
  does not distract from the representation story.
- Keep sampling as a brief practical extension, because it makes expected-value
  readout usable for models without logprobs.
- Treat spread as exploratory/supporting or move it to supplement/companion notes.

What disentangling buys us:

1. A cleaner Paper 1 spine: construct-dependent representation plus failure
   diagnosis.
2. A stronger role for the axis probe, which directly supports that spine.
3. Less pressure to make the distribution results carry theoretical weight they
   were not designed to carry in this paper.
4. A clear decision point: either preserve the distribution findings as a compact
   methods add-on inside Paper 1, or archive them as useful supporting work
   without counting on a second publication.

Current recommendation: promote the BGE axis probe into the main representation
paper. Keep distributional mean/readout results only if they can be made compact
and do not disturb the representation spine. Demote or remove spread unless the
manuscript has room for a clearly secondary measurement section. Do not assume a
Perspective will be available as the natural home for the removed material.


---

## Paper 1 / Paper 2 Split — DECISION (settled)

**Decision: split.** The distribution work leaves Paper 1.

- **Paper 1 (representation paper):** elicitation vs. embedding regression; lexical
  moderators; failure-mode frame; axis probe as the second beat. This is the whole
  paper and it stands on its own.
- **Paper 2 (measurement paper / methods note):** LLM response distributions as
  norming estimates — readout-as-estimator, sampling without log-probabilities,
  spread as an analogue of human disagreement. Its own object, its own audience.

**Why the split is now safe.** Before the probe, the distribution work was Paper 1's
de-facto second beat, which is why it kept reading as "tacked on" — it switched the
object of the paper from *which information is available to which route* to *how to
read an LLM's output*. The axis probe is a better second beat: it stays inside the
representation question and deepens it. Paper 1 no longer needs the distribution work
to feel complete, so the material is free to become its own paper instead of a
demoted third section that compromises both.

**The one seam, and how it is handled.** The expected-value readout *improves
elicitation's numbers*, and Paper 1's core comparison is elicitation vs. embeddings.
Benchmarking elicitation at its top-token weakest invites a fair-comparison
objection. Therefore:

- Paper 1 **keeps only the expected-value readout**, as a one-paragraph methods
  justification: it is the like-for-like match to a human mean (a mean over raters)
  and is the fair readout for the LLM comparison. Used on the two logprob-capable
  models. The *choice* is defended; the *question* is not opened.
- Paper 1 **drops sampling and spread entirely.** Sampling only matters because you
  want the expected value without log-probabilities, which is a Paper 2 question;
  including it reopens the measurement-mechanics door the split is meant to close.
  (This overrides the earlier "keep sampling as a brief practical extension" note.)

**Discipline (non-negotiable).** Paper 1 must read as complete with the distribution
work *absent*, not *deferred*. No forward-references to Paper 2 anywhere in Paper 1.
If the companion is never written, Paper 1 is still whole. Do not shape Paper 1
around an imagined Paper 2 rescue.

## Resection Checklist (applies to the main handoff doc)

The earlier handoff files were written for the integrated two-part paper. To resect
Paper 1:

1. **Abstract** — cut the third paragraph (distribution: probability-weighted mean,
   sampling, spread). Replace with the scoped axis-probe result as the second beat.
   Resolve whether ridge-beats-LLM is stated as a *finding* (against expectation),
   not a ranking.
2. **Three-questions block → two questions.** Keep the representation question. Drop
   the spread question wholesale (Paper 2). The extraction question is no longer a
   research question; it collapses into a one-paragraph methods justification for the
   expected-value readout.
3. **Add the axis probe as Paper 1's second beat** — benchmark establishes the
   boundary; failure-mode frame interprets it; probe shows a subset of embedding
   advantages survive without supervision. Compact: one table (the probe-over-LLM
   dimensions, post-bootstrap), full 18-dim and pole-size tables to supplement.
4. **Methods** — keep the expected-value readout paragraph as a methods
   justification only. Delete the three-readout framing, the sampling subsection, and
   the spread/sampling entries in Statistical Approach. Add the probe method
   (currently in this doc) to Methods.
5. **Contract** — retire the Part B apparatus (readout trio as an axis, mean/spread
   distinction, logprob-capable framing as a research-question driver). Keep
   "expected value" only as the defended readout choice. The failure-mode frame,
   usage/experience-encoded, and the construct-dependent boundary all stay.
6. **Carry to Paper 2 (archive, do not delete):** the readout trio, sampling-without-
   logprobs, spread-vs-disagreement, and all associated contract vocabulary.

## Outstanding before any of this hardens

- Paired bootstrap on the six probe-over-LLM dimensions (4-vs-6 question).
- Ridge-beats-LLM: claim as finding vs. leave as ranking (abstract decision).
- Confirm probe pattern stable across n = 20/30/50/100 (n = 50 primary).
