# Concept embeddings (concept2vec)

`train_concept2vec.py` learns a dense vector for every Strong's number by
treating each verse as a sentence of Strong's IDs and running **skip-gram with
negative sampling** over the whole corpus — implemented in pure NumPy, no gensim
or torch.

```
python3 ml/train_concept2vec.py            # defaults; reads data/kjv.jsonl
python3 ml/train_concept2vec.py --dim 128 --epochs 8 --window 5
```

Output: `data/concept-vectors.vec` in word2vec text format (`<vocab> <dim>`
header, then `STRONGS v1 … vd` per line). It is a gitignored build artifact, like
`data/concept-cache.json`; rebuild it any time. The Haskell side
([src/Overlay/Embed.hs](../src/Overlay/Embed.hs)) loads it into `Env` and degrades
gracefully to the symbolic co-occurrence layer when it is absent.

## Why it can't be "poisoned" by modern English

The only training signal is `data/kjv.jsonl` and its original-language Strong's
tags. The model never sees the 1769 English surface, so it cannot pick up the
modern senses of "prevent", "let", or "meat" the way an off-the-shelf encoder
does. And because no third-party model or corpus is involved, the vectors are
**wholly owned** and free to relicense.

Hebrew (`H…`) and Greek (`G…`) numbers share the vector space but never share a
verse, so cross-language cosines are meaningless; neighbour search is restricted
to the query's own language.

## What it learns (sanity check, printed at the end of a run)

On the real corpus the nearest neighbours recover genuine
collocational/theological structure, e.g.:

- **H1285** *covenant* → H3772 *karath* (cut), H6565 *parar* (break), H7621
  *shebuah* (oath), H423 *alah* (oath/curse) — the "cut/break a covenant" idioms.
- **H2617** *chesed* → H530 *emunah* (faithfulness), H571 *emeth* (truth), H6666
  *tsedeq* (righteousness) — the covenant-virtue word pairs.
- **G26** *agape* → endurance / hope / meekness / comfort (the 1 Cor 13 cluster).
- **G40** *hagios* → G4151 *pneuma* (Holy Spirit); **G4102** *pistis* →
  obedience / justify / unbelief.
- *Verses like* Gen 1:1 → Gen 2:4 and the "heaven and earth" creation verses.

Ultra-frequent tokens (LORD, God) have diffuse vectors — expected, since they
co-occur with everything; the meaningful signal is in the content words. Cosines
run high overall (small-corpus anisotropy); the *ranking* is what the UI uses.
