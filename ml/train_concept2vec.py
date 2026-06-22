#!/usr/bin/env python3
"""Train concept embeddings ("concept2vec") over the KJV.

Each verse is treated as a sentence whose "words" are the Strong's numbers of
its tagged tokens (``H7225 H430 H1254 …`` for Genesis 1:1). Skip-gram with
negative sampling — implemented in pure NumPy, no gensim/torch — then learns one
dense vector per Strong's number, so that concepts appearing in similar contexts
land near each other.

Why this is era-faithful *by construction*: the only training signal is
``data/kjv.jsonl`` and its original-language Strong's tags. The model never sees
the 1769 English surface, so it cannot drift toward modern English the way an
off-the-shelf encoder does ("prevent", "let", "meat" …). And because no
third-party model or corpus is involved, the resulting vectors are wholly owned
and free to relicense.

Output: ``data/concept-vectors.vec`` in word2vec text format::

    <vocab_size> <dim>
    H7225 0.0123 -0.4560 …
    H430  …

The file is a gitignored build artifact (like ``data/concept-cache.json``);
rebuild it any time with this script. The Haskell side loads it into ``Env`` and
degrades gracefully to symbolic co-occurrence when it is absent.

Usage::

    python3 ml/train_concept2vec.py            # defaults; reads data/kjv.jsonl
    python3 ml/train_concept2vec.py --dim 128 --epochs 8
"""

import argparse
import json
import time
from collections import Counter

import numpy as np


def load_sentences(path):
    """Each verse → its list of Strong's numbers, in token order.

    A token may carry several Strong's numbers (rare); all are kept, in order,
    matching the Haskell corpus's ``tokStrongs``. Untagged function words drop
    out, which is exactly what we want for concept context.
    """
    sentences = []
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            obj = json.loads(line)
            toks = obj.get("t")
            if not toks:  # the header line (metadata) has no tokens
                continue
            seq = [s for tok in toks for s in tok[3]]
            if len(seq) >= 2:
                sentences.append(seq)
    return sentences


def sigmoid(x):
    return 1.0 / (1.0 + np.exp(-np.clip(x, -30.0, 30.0)))


def build_pairs(isents, window, rng):
    """(center, context) index pairs with a dynamic window 1…``window``."""
    centers, contexts = [], []
    for idx in isents:
        n = len(idx)
        # one random window radius per centre word (word2vec's reduced window)
        bs = rng.integers(1, window + 1, size=n)
        for i in range(n):
            b = bs[i]
            lo, hi = max(0, i - b), min(n, i + b + 1)
            ci = idx[i]
            for j in range(lo, hi):
                if j != i:
                    centers.append(ci)
                    contexts.append(idx[j])
    return np.asarray(centers, dtype=np.int64), np.asarray(contexts, dtype=np.int64)


def train(args):
    rng = np.random.default_rng(args.seed)

    print(f"reading {args.input} …", flush=True)
    sentences = load_sentences(args.input)
    print(f"  {len(sentences)} verses with ≥2 tagged tokens", flush=True)

    # vocabulary, most-frequent first (index 0 = commonest concept)
    counts = Counter(s for sent in sentences for s in sent)
    vocab = sorted((w for w, c in counts.items() if c >= args.min_count),
                   key=lambda w: -counts[w])
    w2i = {w: i for i, w in enumerate(vocab)}
    V = len(vocab)
    freq = np.array([counts[w] for w in vocab], dtype=np.float64)
    total = freq.sum()
    print(f"  vocab {V} Strong's numbers (min-count {args.min_count}), "
          f"{int(total)} tagged tokens", flush=True)

    # frequent-word subsampling (word2vec): keep prob per word
    if args.sample > 0:
        f = freq / total
        p_keep = np.minimum((np.sqrt(f / args.sample) + 1) * (args.sample / f), 1.0)
    else:
        p_keep = np.ones(V)

    # map sentences to indices, dropping subsampled tokens
    isents = []
    for sent in sentences:
        idx = [w2i[w] for w in sent if w in w2i]
        if args.sample > 0 and idx:
            keep = rng.random(len(idx)) < p_keep[idx]
            idx = [i for i, k in zip(idx, keep) if k]
        if len(idx) >= 2:
            isents.append(idx)

    centers, contexts = build_pairs(isents, args.window, rng)
    P = len(centers)
    print(f"  {P} training pairs", flush=True)

    # unigram^0.75 noise distribution for negative sampling
    noise = freq ** 0.75
    noise /= noise.sum()

    # in/out vectors (word2vec init: input small random, output zeros)
    W_in = ((rng.random((V, args.dim), dtype=np.float32) - 0.5) / args.dim)
    W_out = np.zeros((V, args.dim), dtype=np.float32)

    K, B = args.neg, args.batch
    print(f"training: dim {args.dim}, window {args.window}, neg {K}, "
          f"epochs {args.epochs}", flush=True)
    for epoch in range(args.epochs):
        t0 = time.time()
        perm = rng.permutation(P)
        # linear learning-rate decay across the whole run
        lr = max(args.lr * (1 - epoch / args.epochs), args.lr * 1e-4)
        loss_sum, loss_n = 0.0, 0
        for start in range(0, P, B):
            bi = perm[start:start + B]
            c, o = centers[bi], contexts[bi]
            neg = rng.choice(V, size=(len(c), K), p=noise)

            vc = W_in[c]                              # (b, d)
            uo = W_out[o]                             # (b, d)
            un = W_out[neg]                           # (b, K, d)

            pos = sigmoid(np.einsum("bd,bd->b", vc, uo))      # (b,)
            negs = sigmoid(np.einsum("bd,bkd->bk", vc, un))   # (b, K)

            # gradients (label 1 for the true context, 0 for negatives)
            g_pos = (pos - 1.0)[:, None]              # (b, 1)
            g_neg = negs                             # (b, K)
            grad_vc = g_pos * uo + np.einsum("bk,bkd->bd", g_neg, un)

            # scatter-updates (centres/contexts can repeat within a batch)
            np.add.at(W_in, c, -lr * grad_vc)
            np.add.at(W_out, o, -lr * (g_pos * vc))
            np.add.at(W_out, neg.reshape(-1),
                      -lr * (g_neg.reshape(-1, 1) * np.repeat(vc, K, axis=0)))

            # cheap running objective on a slice
            loss_sum += -(np.log(pos + 1e-9).sum()
                          + np.log(1 - negs + 1e-9).sum())
            loss_n += len(c) * (K + 1)
        print(f"  epoch {epoch + 1}/{args.epochs}  lr {lr:.4f}  "
              f"loss {loss_sum / max(loss_n, 1):.4f}  {time.time() - t0:.1f}s",
              flush=True)

    write_vectors(args.output, vocab, W_in)
    print(f"wrote {V} vectors → {args.output}", flush=True)
    report(W_in, vocab, w2i, args.glosses)


def write_vectors(path, vocab, W):
    with open(path, "w", encoding="utf-8") as fh:
        fh.write(f"{len(vocab)} {W.shape[1]}\n")
        for i, w in enumerate(vocab):
            fh.write(w + " " + " ".join(f"{x:.5f}" for x in W[i]) + "\n")


def report(W, vocab, w2i, gloss_path):
    """Print nearest neighbours for a few well-known concepts, as a smell test."""
    glosses = {}
    try:
        raw = json.load(open(gloss_path, encoding="utf-8"))
        glosses = {k: (v.get("kjv_def") or v.get("strongs_def") or "").strip()
                   for k, v in raw.items()}
    except (OSError, ValueError):
        pass

    norm = W / (np.linalg.norm(W, axis=1, keepdims=True) + 1e-9)

    def gloss(w):
        return glosses.get(w, "")

    seeds = ["H1285", "H3068", "H430", "H157", "H7225", "H2617",
             "G26", "G2316", "G40", "G4102"]
    print("\nnearest concepts (cosine) — sanity check:")
    for w in seeds:
        if w not in w2i:
            continue
        q = norm[w2i[w]]
        sims = norm @ q
        order = np.argsort(-sims)
        nbrs = [(vocab[i], sims[i]) for i in order if i != w2i[w]][:8]
        print(f"\n  {w} = {gloss(w)!r}")
        for nw, s in nbrs:
            print(f"      {s:.3f}  {nw:<7} {gloss(nw)}")


def main():
    ap = argparse.ArgumentParser(description=__doc__,
                                 formatter_class=argparse.RawDescriptionHelpFormatter)
    ap.add_argument("--input", default="data/kjv.jsonl")
    ap.add_argument("--output", default="data/concept-vectors.vec")
    ap.add_argument("--glosses", default="data/strongs.json",
                    help="Strong's lexicon for the sanity report (optional)")
    ap.add_argument("--dim", type=int, default=100)
    ap.add_argument("--window", type=int, default=5)
    ap.add_argument("--neg", type=int, default=5)
    ap.add_argument("--epochs", type=int, default=5)
    ap.add_argument("--min-count", type=int, default=1, dest="min_count")
    ap.add_argument("--sample", type=float, default=1e-3,
                    help="frequent-word subsampling threshold (0 disables)")
    ap.add_argument("--lr", type=float, default=0.025)
    ap.add_argument("--batch", type=int, default=8192)
    ap.add_argument("--seed", type=int, default=42)
    train(ap.parse_args())


if __name__ == "__main__":
    main()
