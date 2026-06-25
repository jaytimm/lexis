#!/usr/bin/env python3
# data-raw/build_wordfreq.py
# Extracts Zipf word frequencies from the wordfreq package for every word
# in lexis_wide. Uses the "large" wordlist where available.
#
# Prerequisites:
#   pip install wordfreq
#
# Citation:
#   Speer, R. (2023). wordfreq: v3.1.1 [Software]. Zenodo.
#   https://doi.org/10.5281/zenodo.7199437
#
# Input:  data-raw/_build/lexis_wide.csv   (produced by build_lexis.R)
# Output: data-raw/_build/wordfreq.csv

import csv
import os
import sys

try:
    from wordfreq import zipf_frequency
except ImportError:
    sys.exit("wordfreq not found. Run: pip install wordfreq")

# ── paths ──────────────────────────────────────────────────────────────────────
script_dir = os.path.dirname(os.path.abspath(__file__))
build_dir  = os.path.join(script_dir, "_build")
input_csv  = os.path.join(build_dir, "lexis_wide.csv")
output_csv = os.path.join(build_dir, "wordfreq.csv")

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

# ── query wordfreq ─────────────────────────────────────────────────────────────
# zipf_frequency returns 0.0 for unknown words
# wordlist="large" uses the larger token list where available for English
rows = []
n_zero = 0
for word in words:
    zf = zipf_frequency(word, "en", wordlist="large")
    if zf == 0.0:
        n_zero += 1
    rows.append({"word": word, "wf_zipf": zf})

# ── write output ───────────────────────────────────────────────────────────────
os.makedirs(build_dir, exist_ok=True)

with open(output_csv, "w", newline="", encoding="utf-8") as f:
    writer = csv.DictWriter(f, fieldnames=["word", "wf_zipf"])
    writer.writeheader()
    writer.writerows(rows)

n_found = len(rows) - n_zero
print(f"Done.")
print(f"  Total words  : {len(rows)}")
print(f"  Found (> 0)  : {n_found} ({100 * n_found / len(rows):.1f}%)")
print(f"  Not found    : {n_zero} ({100 * n_zero / len(rows):.1f}%)")
print(f"  Output       : {output_csv}")
