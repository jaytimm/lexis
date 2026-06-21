# Failure-Mode Frame: Robustness and the Unsupervised Probe

A companion to the main handoff. Records (a) how the failure-mode frame can be criticized, and (b) the unsupervised embedding probe that closes the most serious gap.

---

## 1. The frame, restated

An elicitation miss has three **failure modes**, each a prediction about the joint pattern of the two methods:

| Methods | Norm reliability | Mode | Meaning |
| --- | --- | --- | --- |
| **diverge** | (not needed) | **access** | construct is in text but not expressible as a rating |
| **converge** | low | **noise** | the published norm is unreliable |
| **converge** | high | **difficulty** | construct is not recoverable from text by any method |

- **divergence** (embeddings recover what elicitation misses) is the signature of access.
- **convergent failure** (both fail) is the signature of noise *or* difficulty.
- Two observations resolve three modes: divergence-vs-convergence splits access off; reliability splits noise from difficulty within the convergent branch.

This much is well-identified for the **access vs. noise** contrast: reliability is measured directly, and divergence is observed.

---

## 2. Where the frame is vulnerable

Ordered worst to mildest.

### 2.1 Supervision as an alternative source of divergence — PRIORITY

The frame reads divergence as the signature of access. But the two methods differ in another way that has nothing to do with what text carries: **embedding regression is supervised** (ridge fit on 4,915 labeled words), while **elicitation is zero-shot**.

A supervised model can win for reasons unrelated to access:
- it can learn the norm's marginal distribution and scale usage,
- it can exploit dataset-specific annotation conventions,
- it can fit regularities specific to how that norm set was built.

So embeddings could beat elicitation on auditory simply because ridge saw 4,915 auditory labels and the LLM never did — a divergence driven by **supervision**, not access. As written, the frame does not distinguish these. This is the only criticism that offers a *complete alternative reading* of the headline result, not just a caveat.

### 2.2 Difficulty is a residual-only category

The noise/difficulty split rests entirely on the reliability estimate, and difficulty is never positively tested — it is only inferred (convergence + high reliability). The reliability estimate itself has a soft spot: for the Lancaster norms, per-item rater counts are not reported and `N_known` is substituted. If reliability is off, the noise/difficulty split weakens. (This does **not** touch the access claim, which rests on divergence, not reliability.)

### 2.3 "Expressible" is a theoretical commitment, not an empirical result

Access is defined as "present in text but not expressible as a rating." The divergence data do not prove *why* a text-trained model cannot voice something its own training text contains. The grounded-cognition account (it can voice what is stabilized in usage, not what survives only as a faint distributional trace) is plausible but is an interpretation. A reviewer can accept the identification and still contest the mechanism. This is an interpretation question, not an identification hole.

### 2.4 Convergent failure has a shared-blind-spot variant (minor)

Both methods could fail together because they inherit the same gap from English text — neither "noise" nor "unrecoverable in principle." In practice this collapses into the difficulty mode, but difficulty should be read as "not in *this* text by *these* methods," not "unrecoverable in principle."

### Net assessment

The frame is safe for the **access vs. noise** contrast. The soft spots are supervision (2.1, a potential reframe), difficulty as residual-only (2.2), and the theoretical status of *why* access fails (2.3). Only 2.1 needs active closing.

---

## 3. Closing the supervision confound

Four options, weakest to strongest. Always do Option 4; lead with Option 3; add Option 2 if there is appetite.

### Option 1 — Redefine access to absorb supervision (cheap, a retreat)

Define access as: *recoverable from the distributional signal by some text-based method, but not by elicitation.* Under this definition, "embeddings learned it via supervision" no longer undercuts access — it demonstrates the signal was in the embeddings (in the text) to be learned. Cost: the claim weakens from "elicitation can't access usage-encoded knowledge" to "elicitation can't access it the way a fitted model can," which retreats from the grounded-cognition story. Honest and defensible, but only if stated out loud — do not do this silently.

### Option 2 — Unsupervised embedding probe (strongest empirical answer)

See §4. Removes supervision entirely: predict the norm from embedding geometry with no fitting to the labels. If even this crude probe beats elicitation where embeddings win, the advantage cannot be supervision. Converts the defense from "we argue" to "we show."

### Option 3 — Symmetry argument (free, load-bearing)

Embeddings are supervised on **all 18 dimensions** yet **lose on five** (valence, arousal, AoA, gustatory, olfactory). If supervision drove divergence, embeddings should win wherever they are trained — i.e., everywhere. They do not. So the divergence pattern tracks something about the construct, not the mere presence of supervision. Uses data already in hand. Does not fully kill the criticism (supervision could help *variably* across constructs), but badly weakens the simple version.

### Option 4 — Honest limitation paragraph (floor, never skip)

Name the asymmetry, scope the claim, cite the symmetry argument as evidence supervision is not the primary driver. Required regardless of what else is done.

### Recommendation

**3 + 4**, plus **2** if the probe is run. The symmetry argument is the load-bearing free move; the probe upgrades it to reviewer-proof.

---

## 4. The unsupervised probe — method

Goal: predict each norm from embedding geometry with **no fitting to the norm labels**. Labels are used only to locate the two ends of the scale.

**Per dimension:**

1. **Find the poles (training words only).** Sort the 4,915 training words by their human rating. Take the top *n* and bottom *n* (n ≈ 20–50).
2. **Build pole vectors.** Average the top-*n* embeddings → `high`; average the bottom-*n* → `low`. (L2-normalize vectors first — already done for BGE; do it for GloVe too.)
3. **Make the axis.** `axis = high − low`: a single direction from the low end to the high end. This *is* the unsupervised scale.
4. **Score holdout words.** For each holdout word, project onto the axis: `score = word_vector · axis`. One number per word.
5. **Evaluate.** Correlate the 1,638 holdout scores with the 1,638 human ratings → one Pearson *r*.

**Critical constraint:** poles come from the **training split only**; the holdout is never used to locate them. Same train/holdout boundary as the ridge models, so the comparison is fair and there is no leakage. Labels locate the poles; they do not fit a mapping — that is the sense in which it is unsupervised.

```python
import numpy as np

def unsupervised_probe(E_train, y_train, E_hold, n=30):
    # E_train: (n_train, d) embeddings, L2-normalized
    # y_train: (n_train,) human ratings
    # E_hold:  (n_hold, d) embeddings, L2-normalized
    order = np.argsort(y_train)
    low  = E_train[order[:n]].mean(axis=0)
    high = E_train[order[-n:]].mean(axis=0)
    axis = high - low
    return E_hold @ axis            # correlate with held-out human ratings
```

Evaluate with `np.corrcoef(scores, y_hold)[0, 1]` per dimension.

---

## 5. Reading the probe result

**Run it first on the dimensions where the argument lives:** where embeddings beat elicitation *and* reliability is high — auditory, interoceptive, mouth. That is where "access" is the headline.

**If the probe beats elicitation there:** the auditory signal is in the text (a method that never trained on the norm recovers it), and elicitation still loses. Difficulty is ruled out (signal is present); supervision is ruled out (no fitting). What remains is access. The probe strengthens the access claim **by elimination** — it removes the supervision confound. It does **not** supply a positive mechanism for why elicitation can't voice it; that stays the grounded-cognition interpretation.

**If the probe loses to elicitation there:** access is not disproven, but supervision can no longer be ruled out by this route. Fall back to the symmetry argument (Option 3) plus the limitation paragraph (Option 4). The probe is **upside-only** — its failure just returns you to where you already were.

### Pre-commit to the framing

The probe will score **lower than ridge everywhere** — it is a weaker predictor by design (ridge fits; the probe does not). That is expected and not the point. The comparison that matters is **probe vs. elicitation**, never probe vs. ridge. State this up front, or a reviewer will say "your probe is worse than ridge, so what" and miss that *worse-than-ridge-but-better-than-elicitation* is exactly the result the access argument needs.

---

## 6. What the probe does and does not establish

- **Does:** removes the supervision confound; with it gone, divergence cleanly indicates access; rules out difficulty on the tested dimensions (signal demonstrably present in text).
- **Does not:** explain *why* a text-trained LLM cannot voice a construct its training text contains (interpretation, not identification); resolve noise vs. difficulty on *converging* dimensions (still needs reliability); prove the grounded-cognition mechanism.
