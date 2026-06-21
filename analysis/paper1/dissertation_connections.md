# Dissertation (Timm 2016) → Present Study: Ideas and Language

Connections between the dissertation and the norming benchmark paper, ordered by strength. Each entry: the idea, its 2016 source, where it lands in the present paper, and status.

---

## 1. Speaker vs. speech community → top token vs. distribution

**The strongest connection; candidate for inclusion in the paper.**

- 2016 source: the dissertation's stated methodological contribution — "By disentangling speaker from speech community methodologically, we were able to demonstrate important differences..."
- Mapping: the standard top-token procedure treats the LLM as a single simulated speaker; reading the response distribution treats it as a sample from a simulated speech community.
- Why it works: a norm is a community-level object (a mean over raters), so the community reading is the like-for-like comparison. Restates the mean-over-raters argument in established vocabulary.
- Bonus: motivates the second moment for free. A community has internal variation; a calibrated community-sample should disagree where the community disagrees. Deepest available motivation for the spread analysis.
- Candidate positions: one sentence in intro question 2; or Discussion. [Draft both, pick one.]

## 2. Ratings sourced in linguistic experience → norm drift as residual misalignment

**The 2016 piece.** Clark & Paivio observed that norms collected from different generations of raters differ — the same words rated differently in 1968 and 2004. The dissertation's gloss: "If we assume that such ratings are sourced in linguistic experience, then this particular finding is ultimately predicted." Ratings are not readings off a fixed lexicon; they are functions of the experience the rater accumulated.

**The application.** The benchmark compares LLM ratings against norms collected at specific moments from specific cohorts (Warriner ~2012; Lancaster ~2019). The LLM's "linguistic experience" is its training corpus: decades of text, recency-skewed, written register. By the 2016 logic, the model is effectively a rater from a different generation and register community. Some disagreement is then predicted even on well-captured dimensions — not method failure but experiential mismatch. Concrete cases: the valence of *viral*, *tweet*, *sick* for a 2012 MTurk cohort vs. a corpus centered later.

**Why it earns a Discussion paragraph.** It gives the residual a mechanism instead of "models are imperfect," and the mechanism is falsifiable: misalignment should concentrate on words with recent semantic or evaluative change. It also sets a ceiling argument — perfect agreement with a 2012 norm is not the right target for a system trained on later text.

**Quantitative anchor (Clark & Paivio, 2004).** The drift is measured, not just asserted: imagery ratings collected in 2001–2002 correlated .803 with the original 1968 PYM ratings; replication familiarity correlated .852 with Paivio (1974); and the familiarity shift was systematic (older ratings lower until ceiling), i.e., directional drift. Interpretive payoff: two human cohorts ~30 years apart agree at r ≈ .80–.85, so the benchmark's best mean r (.734) should be read against the human cross-generational ceiling, not against 1.0. Different dimensions and designs — frame as context, not equivalence.

**Forward flip (future work).** Clark & Paivio frame replications as tests of the "continuing validity" of aging norms. A calibrated LLM rater inverts this into a cheap re-norming instrument: disagreement concentrated on particular words becomes a drift detector flagging re-norming candidates.

**Cautionary cite.** Anachronistic application of contemporary norms is routine practice — e.g., Hills & Adelman (2015) score 200 years of American English with present-day concreteness ratings, presuming the cross-generational stability of ratings that the drift evidence denies. Citation function: example of the practice the mechanism problematizes, not endorsement. Contrast available: the dissertation's proxy logic (item 3) exists precisely to avoid this — learn the rating mapping synchronically, deploy distributionally, rather than projecting modern ratings backward raw.

**One-sentence version:** the human norms are themselves generation-stamped, so part of the LLM-human gap is generational mismatch, not model error — argued in 2016 before there were LLMs to apply it to.

## 3. Distribution-as-proxy lineage (factor analysis ≈ embedding regression)

**The 2016 piece.** Behavioral data (ratings, lexical decision times) exist only for living speakers; there is no lexical decision for the speech community of 1850. The move: on contemporary data, where distributional and behavioral measurements coexist, establish their relationship (the factor analysis: shared constructs of accessibility, familiarity, informativity). Then, in historical periods where only corpora survive, let distributional features stand proxy for the cognitive constructs. Learn the link where both data types exist; deploy it where only one does.

**The application.** Embedding regression for norm extension is the identical template with the axis swapped. Ratings exist for a subset of words; embeddings exist for all words; fit the mapping where both exist, deploy where only embeddings do. 2016 extended along the time axis (no behavioral data for the past); norm extension extends along the coverage axis (no ratings for most of the lexicon).

**Why it earns a sentence.** Reframes the embedding route from "ML baseline" to a method with a usage-based lineage predating embeddings. Carries a caution worth inheriting: the proxy is only as good as the learned mapping — supervised regression can only recover what its training ratings already encode, whereas elicitation is not tethered to human ground truth. That asymmetry is a clean Discussion-level explanation for why the two methods can dissociate at all.

**One-sentence version:** the embedding route is the dissertation's distributional-proxy logic applied along the coverage axis instead of the time axis.

## 4. Possible new analysis: factor the 18 dimensions

- 2016 source: accessibility / familiarity / informativity construct structure from the dissertation's factor analysis.
- Idea: factor the 18 dimensions' alignment profiles; test whether the "evaluative and experience-linked cluster" (valence, arousal, AoA, gustatory, olfactory) falls out as a construct family rather than an enumerated list.
- Status: cheap if the per-dimension results are in hand; would upgrade the cluster from description to structure. [Decide: run, or flag as future work.]
- Caveat if run (Clark & Paivio, 2004): variance explained by a factor reflects how many measures of that construct happen to be in the set, not the construct's importance — the 18 dimensions sample construct families unevenly (many sensorimotor measures, few affective), so factor sizes will partly mirror sampling. Their recommended posture: overextract (Wood et al., 1996).

## 5. The polysemy null has a 2016 explanation

- 2016 source: degree of polysemy loaded on accessibility (the frequency factor), against the expectation that it pattern with informativity.
- Mapping: if polysemy is largely frequency-shadowed, it has little independent purchase in mixed models with frequency present — explaining why polysemy is not a reliable moderator in the present study.
- Use: one Discussion sentence; self-citation available.

## 6. Smaller items

- **Terminology**: "behavioral features" vs. "distributional features" — established two-modality vocabulary; could standardize the present paper's terms for rating-derived vs. corpus-derived variables.
- **AoA thread — resolved (Clark & Paivio, 2004).** AoA sits in the LLM-favored cluster, which looks anomalous for a developmental dimension. Clark & Paivio's factor analysis explains it: AoA loaded "rather evenly" across familiarity, length, concreteness, and frequency, and they speculate that raters judging AoA "are actually making judgments of how concrete, short, and familiar items are." If AoA ratings are composite judgments built from accessible cues rather than autobiographical memory of learning, AoA is text-recoverable by construction — its place in the LLM cluster is predicted, not anomalous. One citable Discussion sentence.

---

## 7. References worth reviving (from the 2016 list)

Ranked by obscurity-to-usefulness ratio; one line each on the job it does in the present paper.

- **Schmid & Mantlik (2015), "Entrenchment in historical corpora? Reconstructing dead authors' minds from their usage profiles" (Anglia).** Limiting case of distribution-as-proxy (item 3): if usage profiles can stand in for the minds of the dead, extending norms to unrated words is the easy version of the problem.
- **Blumenthal-Dramé (2012), Entrenchment in usage-based theories: what corpus data do and do not reveal about the mind.** The subtitle is the paper's research question; pre-states the boundary framing a decade before LLMs made it testable. Intro or Discussion anchor.
- **Schmid (2010), "Does frequency in text instantiate entrenchment in the cognitive system?"** Article-form sibling of the above.
- **Kilgarriff (1997), "I don't believe in word senses" / Hanks (2000), "Do word meanings exist?"** Sense-skepticism: a rating of a polysemous word is an average over sense-uses, so rater disagreement partly reflects sense sampling — feeds the spread analysis, plus the standard polysemy-operationalization caveat.
- **Fenk-Oczlon & Fenk (2010), "Frequency effects on the emergence of polysemy and homophony."** Diachronic mechanism behind the polysemy null (item 5): frequency generates polysemy, so polysemy is frequency-shadowed synchronically.
- **McDonald & Shillcock (2001), "Rethinking the word frequency effect."** Distribution-beyond-frequency predicting processing: embedding regression's thesis pre-embeddings; lineage cite alongside Recchia & Jones.
- **Nelson, McEvoy & Dennis (2000), "What is free association and what does it measure?"** What-does-the-instrument-measure genre; pairs with Clark & Paivio's AoA-as-composite point to support the norms-as-rating-behavior ontology.
- **Britton (1978).** The 44%-polysemous estimate, in BRM&I — venue lineage if submitting to BRM.
- **Köhler (1986), Zur linguistischen Synergetik.** Deep cut: the lexicon as a self-regulating system of mutually constraining properties — theoretical justification for correlated, competing moderators. Cite only if prepared to defend it.

---

## 8. Addendum: speech community as a scoping principle

Origin: hesitation about using the Glasgow Norms (Scottish university raters) in an American English-focused design. The discussion upgraded a half-embarrassed avoidance into a principled scoping rule, and then into a research direction.

**The fact check first.** The "33 college kids" worry is misaimed: ~33 is raters per word, from a total pool of ~800+; per-word n exceeds Warriner (~20) and Kuperman. On reliability, Glasgow is among the cleanest sets available, and its published convergence with American norms on shared words is high [verify exact values: Scott et al., 2019, validation correlations vs. Warriner / Kuperman / Brysbaert]. Sample size is not the argument.

**The actual argument.** Norms are sourced in linguistic experience (Timm, 2016), and speech community membership is part of that experience. Dialect mismatch is the synchronic analogue of cohort mismatch: the drift paper's axis is time, this one is space, and a norm is stamped with both. Aggregate cross-set correlations of ~.9 are the wrong comfort for word-level designs, because the divergence concentrates in a minority of items (biscuit, jumper, surgery, scheme; familiarity and AoA most dialect-sensitive, valence on culturally loaded items) — precisely the regions a stratified sample reaches into.

**Benchmark-specific force.** Dimension-level comparisons are the core result; if dimensions are sourced from different rater populations, dimension differences are partially population differences. The LLMs' training diet is American-skewed, so an apparent construct effect could be dialect mismatch in costume. Holding rater population constant across dimensions is a design requirement, not a preference.

**Drop-in language.** "Norm sets were restricted to North American rater populations to hold speech community constant across dimensions; dialect mismatch between raters and training corpora would otherwise confound dimension-level comparisons."

**Cost.** Glasgow-only dimensions (semantic size, gender association) are priced out by the rule. If one becomes essential: use it with population flagged as a known confound, not silently.

**Where the legs are.**
- **Glasgow × Warriner overlap is labeled community-difference data** — the synchronic twin of the Clark & Paivio pairs (labeled time-drift data). Shared words rated by two communities give per-word divergence scores, free.
- **Which community is the model from?** Test whether LLM ratings agree better with American than with Scottish norms on the shared words, concentrating where human communities diverge. Parallel in structure to the drift paper's 2001-vs-1968 cohort test.
- **Steerability.** Whether prompting ("rate as a British English speaker") moves the simulated community toward Glasgow on the diverging items is a self-contained, publishable question — and bears on whether community membership is a parameter of the instrument or a fixed property.
- **The programmatic statement.** Items 1, 2, and 8 are one program: machine raters as samples from specifiable speech communities, located in time and in space. The response distribution gives the sample; cohort and community give its coordinates. If a flag is ever wanted for the research-statement version, this is it.

## Action list

1. Draft the speaker/speech-community sentence in both candidate positions (intro Q2; Discussion); pick one. [item 1]
2. Add norm-drift paragraph to Discussion limitations. [item 2]
3. One lineage clause where embedding regression is introduced. [item 3]
4. Decide on the dimension-level factor analysis: run vs. future work. [item 4]
5. One Discussion sentence on the polysemy null. [item 5]
6. Terminology pass: adopt behavioral/distributional pairing if consistent with current usage. [item 6]
7. Reference revival: pick 3–4 from §7 (Schmid & Mantlik, Blumenthal-Dramé, Kilgarriff/Hanks, Fenk-Oczlon & Fenk are the front-runners); slot at the framing, spread, and polysemy-null points respectively. [item 7]
