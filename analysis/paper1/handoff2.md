# Representation Paper: Benchmark, Failure Modes, and the Axis Probe

## Core Result

The benchmark establishes a construct-dependent boundary in psycholinguistic representation.

Across 18 dimensions, supervised embedding regression outperformed LLM elicitation on 13, despite the LLMs being larger, newer, and far more capable systems overall. The dimensions favoring elicitation were concentrated in a small evaluative and experience-linked cluster (valence, arousal, age of acquisition, gustatory, olfactory), whereas embedding advantages were strongest for abstract and sensorimotor dimensions and increased with word abstractness.

The primary result is therefore not a ranking of methods but a boundary: different computational routes succeed on different classes of psycholinguistic construct.

---

## Failure-Mode Framework

The benchmark raises an interpretive question: what does it mean when embeddings recover a norm that elicitation misses?

The framework distinguishes three possibilities:

| Pattern                               | Interpretation        |
| ------------------------------------- | --------------------- |
| Both methods fail                     | Noise or difficulty   |
| Embeddings succeed, elicitation fails | Divergence            |
| Both methods succeed                  | Shared recoverability |

The original interpretation was that divergence reflects an access failure: the relevant information is present in text representations but is not effectively recovered through rating-style generation.

However, divergence alone does not identify access.

A competing explanation is supervision. Ridge regression is trained on thousands of labeled examples, whereas elicitation is zero-shot. A supervised model can outperform elicitation without revealing anything about representational access.

This is the central vulnerability of the failure-mode account.

---

## What the Axis Probe Adds

The axis probe was introduced to address the supervision alternative.

For each dimension:

* identify high and low poles using training words,
* construct a single embedding axis,
* project holdout words onto that axis,
* correlate projections with human ratings.

The probe uses no fitted regression weights.

The key comparison is not probe versus ridge. The probe is intentionally weaker than ridge.

The key comparison is probe versus elicitation.

---

## A Second Boundary Within the Failure Region

The benchmark produces an initial partition:

```text
18 dimensions

├── LLM favorable (5)
└── Embedding favorable (13)
```

The probe introduces a second partition within the embedding-favorable region:

```text
13 embedding-favorable dimensions

├── Probe > LLM
└── Probe ≤ LLM
```

This is the conceptual contribution of the probe.

The probe is not another competing model family. It decomposes the embedding advantage.

Some embedding advantages survive even when supervision is removed. Others do not.

This reveals structure within the LLM-failure region rather than simply adding another performance comparison.

---

## Why Probe Wins Matter

A ridge win can always be attributed to supervision.

A probe win cannot be dismissed that way.

When a one-dimensional geometric projection outperforms elicitation, the resulting advantage is difficult to explain as a consequence of supervised calibration.

The relevant signal is already present in the geometry of the representation.

The strongest access cases are therefore not the 13 dimensions where ridge beats elicitation. They are the subset of those dimensions where the unfitted probe also beats elicitation.

Conceptually:

```text
Ridge > LLM (13)
        │
        ▼
Candidate access failures

Probe > LLM (subset)
        │
        ▼
Stronger access-failure cases
```

The probe does not establish why elicitation fails. It removes a major alternative explanation.

---

## Beyond a Simple Text-versus-Experience Interpretation

The probe also complicates a simple distinction between text-based and experience-based knowledge.

Many psycholinguistic dimensions are commonly interpreted as grounded in direct sensory or bodily experience. Under a strong version of that view, such information should not be recoverable from text-derived representations.

Yet several dimensions often described in experiential terms remain recoverable from embedding geometry.

This does not imply that those constructs are reducible to linguistic usage.

It does imply that experience-linked information can leave systematic traces in text representations.

The question therefore shifts.

The issue is not only whether information is represented in text. It is also whether a particular retrieval route can access that information.

From this perspective, some elicitation failures appear to be failures of access rather than failures of representation.

---

## Working Interpretation

The benchmark establishes a construct-dependent boundary.

The failure-mode framework provides a way to interpret that boundary.

The axis probe strengthens one branch of that framework by showing that some embedding advantages survive the removal of supervision.

The resulting picture is neither a simple method comparison nor a simple text-versus-experience distinction.

Instead:

1. Different computational routes succeed on different psycholinguistic constructs.
2. Some dimensions commonly viewed as experience-linked remain detectable in text representations.
3. Information can be present in a representation while remaining inaccessible to rating-style generation.
4. Failures of elicitation are heterogeneous rather than uniform.

The probe's main contribution is not that it wins on a handful of dimensions. It is that it reveals structure within the set of dimensions where elicitation fails.

---

# Working Abstract

Computational approximations of psycholinguistic norms now come in two families: ratings elicited from large language models and regressions of human norms on word embeddings. The two are rarely tested on the same items, and existing evaluations rely on small convenience samples skewed toward concrete, frequent words. We benchmarked four LLMs (gpt-4o-mini, gpt-4.1, claude-haiku-4-5, and gpt-5.4-nano) against GloVe-300 and BGE-large-en-v1.5 ridge models on a stratified 1,638-word holdout spanning 18 psycholinguistic dimensions.

Accuracy proved strongly construct-dependent, and not in the expected direction. Despite their scale and recency, the LLMs were outperformed by supervised embedding regression on 13 of 18 dimensions (BGE-large mean r = .734 vs. best LLM r = .636). Elicitation led only on a cluster of evaluative and experience-linked dimensions, including valence, arousal, age of acquisition, gustatory, and olfactory ratings, whereas embedding models performed best on abstract and sensorimotor dimensions and widened their advantage as words became more abstract.

We interpret this construct-dependent boundary through a failure-mode framework that distinguishes access failures from convergent failures. To evaluate whether embedding advantages reflected access rather than supervised calibration, we compared elicitation with an unfitted BGE axis probe. On several dimensions, the probe outperformed elicitation despite fitting no regression weights.

Together, the results support a construct-dependent view of psycholinguistic representation. Elicitation and embedding regression appear to succeed on different classes of psycholinguistic construct, and the probe results suggest that some failures of LLM norm elicitation reflect limitations of access rather than an absence of encoded information.
