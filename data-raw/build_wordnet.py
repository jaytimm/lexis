#!/usr/bin/env python3
# data-raw/build_wordnet.py
# Extracts WordNet sense counts and definitions for every word in lexis_wide.
#
# Prerequisites:
#   pip install nltk
#   python -c "import nltk; nltk.download('wordnet'); nltk.download('omw-1.4')"
#
# Input:  data-raw/_build/lexis_wide.csv   (produced by build_lexis.R)
# Output: data-raw/_build/wordnet_defs.csv

import csv
import os
import sys

try:
    from nltk.corpus import wordnet as wn
except ImportError:
    sys.exit("nltk not found. Run: pip install nltk")

try:
    # trigger load to catch missing data early
    wn.synsets("test")
except LookupError:
    import nltk
    nltk.download("wordnet")
    nltk.download("omw-1.4")
    from nltk.corpus import wordnet as wn

# ── paths ──────────────────────────────────────────────────────────────────────
script_dir = os.path.dirname(os.path.abspath(__file__))
build_dir  = os.path.join(script_dir, "_build")
input_csv  = os.path.join(build_dir, "lexis_wide.csv")
output_csv = os.path.join(build_dir, "wordnet_defs.csv")

if not os.path.exists(input_csv):
    sys.exit(f"Input not found: {input_csv}\nRun build_lexis.R first.")

# ── read unique words ──────────────────────────────────────────────────────────
words = []
with open(input_csv, newline="", encoding="utf-8") as f:
    reader = csv.DictReader(f)
    if "word" not in reader.fieldnames:
        sys.exit(f"'word' column not found in {input_csv}")
    seen = set()
    for row in reader:
        w = row["word"].strip().lower()
        if w and w not in seen:
            seen.add(w)
            words.append(w)

print(f"Words to look up: {len(words)}")

# ── query WordNet ──────────────────────────────────────────────────────────────
POS_MAP = {
    wn.NOUN: "n_noun",
    wn.VERB: "n_verb",
    wn.ADJ:  "n_adj",   # includes adjective satellites (wn.ADJ_SAT)
    wn.ADV:  "n_adv",
}
ADJ_SAT = "s"  # WordNet internal tag for adjective satellites → treated as ADJ

rows = []
for word in words:
    synsets = wn.synsets(word)

    counts = {"n_noun": 0, "n_verb": 0, "n_adj": 0, "n_adv": 0}
    defs   = []

    for syn in synsets:
        pos = syn.pos()
        if pos == ADJ_SAT:
            pos = wn.ADJ
        key = POS_MAP.get(pos)
        if key:
            counts[key] += 1
        defs.append(syn.definition())

    rows.append({
        "word":         word,
        "wn_n_synsets": len(synsets),
        "wn_n_noun":    counts["n_noun"],
        "wn_n_verb":    counts["n_verb"],
        "wn_n_adj":     counts["n_adj"],
        "wn_n_adv":     counts["n_adv"],
        "wn_first_def": defs[0] if defs else "",
        "wn_definitions": " | ".join(defs),
    })

# ── per-synset output ──────────────────────────────────────────────────────────
POS_NAME = {
    wn.NOUN: "noun",
    wn.VERB: "verb",
    wn.ADJ:  "adjective",
    wn.ADV:  "adverb",
}

synset_rows = []
for word in words:
    for rank, syn in enumerate(wn.synsets(word), start=1):
        pos = syn.pos()
        if pos == ADJ_SAT:
            pos = wn.ADJ
        examples = syn.examples()
        synset_rows.append({
            "word":        word,
            "synset_name": syn.name(),
            "pos":         POS_NAME.get(pos, pos),
            "sense_rank":  rank,
            "definition":  syn.definition(),
            "examples":    " | ".join(examples) if examples else "",
        })

# ── write output ───────────────────────────────────────────────────────────────
os.makedirs(build_dir, exist_ok=True)

fieldnames = [
    "word", "wn_n_synsets",
    "wn_n_noun", "wn_n_verb", "wn_n_adj", "wn_n_adv",
    "wn_first_def", "wn_definitions",
]

with open(output_csv, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=fieldnames)
    writer.writeheader()
    writer.writerows(rows)

synsets_csv = os.path.join(build_dir, "wordnet_synsets.csv")
synset_fieldnames = ["word", "synset_name", "pos", "sense_rank", "definition", "examples"]
with open(synsets_csv, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=synset_fieldnames)
    writer.writeheader()
    writer.writerows(synset_rows)

n_found = sum(1 for r in rows if r["wn_n_synsets"] > 0)
print(f"Done.")
print(f"  Total words : {len(rows)}")
print(f"  In WordNet  : {n_found} ({100*n_found/len(rows):.1f}%)")
print(f"  Output      : {output_csv}")
print(f"  Synsets     : {synsets_csv} ({len(synset_rows)} rows)")
