#!/usr/bin/env python3
"""
build-bge-embeddings.py — BGE embeddings for lexis core vocabulary.

Reads the word list from analysis/output/core_vocab_samples_wide.csv,
encodes with BAAI/bge-large-en-v1.5, writes a flat CSV that R reads
directly into a named matrix.

Usage:
    python analysis/build-bge-embeddings.py
    python analysis/build-bge-embeddings.py --model BAAI/bge-base-en-v1.5
    python analysis/build-bge-embeddings.py --batch_size 128

Output:
    analysis/output/bge_large_embeddings.csv
    columns: word, d1, d2, ..., d1024

Requirements:
    pip install sentence-transformers pandas
"""

import argparse
import os
import sys
import numpy as np
import pandas as pd
from sentence_transformers import SentenceTransformer


def parse_args():
    p = argparse.ArgumentParser(description="BGE embeddings for lexis core vocab")
    p.add_argument("--model",      default="BAAI/bge-large-en-v1.5")
    p.add_argument("--batch_size", type=int, default=256)
    p.add_argument("--input",      default="analysis/output/core_vocab_wide.csv")
    p.add_argument("--output",     default="analysis/output/bge_large_embeddings.csv")
    return p.parse_args()


def find_pkg_root():
    cur = os.path.abspath(".")
    while True:
        if os.path.exists(os.path.join(cur, "DESCRIPTION")):
            return cur
        parent = os.path.dirname(cur)
        if parent == cur:
            sys.exit("Could not find package root (no DESCRIPTION found).")
        cur = parent


def main():
    args     = parse_args()
    base_dir = find_pkg_root()
    in_path  = os.path.join(base_dir, args.input)
    out_path = os.path.join(base_dir, args.output)

    if not os.path.exists(in_path):
        sys.exit(f"Missing input: {in_path}")

    words = (
        pd.read_csv(in_path)["word"]
        .drop_duplicates()
        .sort_values()
        .tolist()
    )
    print(f"Words to embed : {len(words)}")

    print(f"Loading model  : {args.model}")
    model = SentenceTransformer(args.model)
    dim   = model.get_sentence_embedding_dimension()
    print(f"Embedding dim  : {dim}")

    print("Encoding ...")
    embs = model.encode(
        words,
        batch_size           = args.batch_size,
        normalize_embeddings = True,
        show_progress_bar    = True
    )

    cols = [f"d{i+1}" for i in range(dim)]
    df   = pd.DataFrame(embs, columns=cols)
    df.insert(0, "word", words)

    df.to_csv(out_path, index=False)
    print(f"Saved          : {out_path}  ({df.shape[0]} x {df.shape[1]})")


if __name__ == "__main__":
    main()
