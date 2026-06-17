#!/usr/bin/env python3
"""Build the LXX Hebrew<->Greek bridge artifact (the OPT-IN cross-testament source).

Self-contained and reproducible: fetches a public-domain Greek Septuagint
(Swete), lemmatizes it with an MIT lemma dictionary, maps lemmas to Greek
Strong's, and learns Hebrew<->Greek translation equivalents from per-verse
co-occurrence against the tagged KJV (Dice score). Output is yours to license
openly (PD text + MIT lemmatizer + Strong's-number facts).

Validated: H3068<->G2962 (YHWH<->kyrios), H430<->G2316 (elohim<->theos),
H4899<->G5547 (Messiah<->Christ), H1285<->G1242 (covenant) all surface strongly.

Sources (cached under --work; fetched once):
  - Swete LXX words + versification: eliranwong/LXX-Swete-1930 (the Swete 1909-1930
    TEXT is public domain; we use only the bare words + factual verse boundaries).
  - Greek lemma dictionary: cltk/grc_models_cltk (MIT), form->lemma.
  - Hebrew Strong's per verse: the tagged KJV in data/kjv.jsonl (public domain).
  - lemma -> Greek Strong's: data/strongs.json (Strong's numbers are facts).

Scope: the protocanonical OT books whose versification matches the KJV. EXCLUDED
(documented, not silent): Psalms (LXX numbering differs — needs a mapping) and the
apocrypha / LXX-only books (no Hebrew counterpart). Co-occurrence also links a few
tightly-bound collocations (fixed name pairs) that aren't true equivalents; the
Dice floor limits this and word-level alignment would refine it.
"""
import argparse, json, pickle, unicodedata, urllib.request
from collections import Counter
from pathlib import Path

SWETE_WORDS = "https://raw.githubusercontent.com/eliranwong/LXX-Swete-1930/master/02-Swete_word_without_punctuations.csv"
SWETE_VERS  = "https://raw.githubusercontent.com/eliranwong/LXX-Swete-1930/master/00-Swete_versification.csv"
LEMMA_PKL   = "https://raw.githubusercontent.com/cltk/grc_models_cltk/master/lemmata/backoff/greek_lemmata_cltk.pickle"

# LXX book label -> KJV book id (protocanonical, KJV-matching versification only)
BMAP = {'Gen':'Gen','Exo':'Exod','Lev':'Lev','Num':'Num','Deu':'Deut','Jos':'Josh',
 'Jdg':'Judg','Rut':'Ruth','1Sa':'1Sam','2Sa':'2Sam','1Ki':'1Kgs','2Ki':'2Kgs',
 '1Ch':'1Chr','2Ch':'2Chr','Ezr':'Ezra','Neh':'Neh','Est':'Esth','Job':'Job',
 'Pro':'Prov','Ecc':'Eccl','Sol':'Song','Isa':'Isa','Jer':'Jer','Lam':'Lam',
 'Eze':'Ezek','Dan':'Dan','Hos':'Hos','Amo':'Amos','Mic':'Mic','Joe':'Joel',
 'Oba':'Obad','Jon':'Jonah','Nah':'Nah','Hab':'Hab','Zep':'Zeph','Hag':'Hag',
 'Zec':'Zech','Mal':'Mal'}
# divine/core forms the context-free dictionary lemmatizes wrongly (theos paradigm)
OVERRIDE = {f: 'G2316' for f in
    ['θεοσ','θεου','θεω','θεον','θεους','θεοισ','θεοι','θεων','θεε']}


def norm(s):
    s = unicodedata.normalize('NFD', s or '')
    return ''.join(c for c in s if unicodedata.category(c) != 'Mn').lower().replace('ς', 'σ')


def fetch(url, dest):
    if not dest.exists():
        dest.parent.mkdir(parents=True, exist_ok=True)
        urllib.request.urlretrieve(url, dest)
    return dest


def main():
    ap = argparse.ArgumentParser(description="Build the LXX H<->G bridge artifact.")
    ap.add_argument("--work", default="pipelines/.lxx-cache", help="download cache dir")
    ap.add_argument("--kjv", default="data/kjv.jsonl")
    ap.add_argument("--strongs", default="data/strongs.json")
    ap.add_argument("--out", default="bridge/lxx-alignment.json")
    ap.add_argument("--min-dice", type=float, default=0.30)
    ap.add_argument("--min-co", type=int, default=3)
    args = ap.parse_args()
    work = Path(args.work)

    cltk = pickle.load(open(fetch(LEMMA_PKL, work / "greek_lemmata.pickle"), "rb"))
    form2lemma = {}
    for f, l in cltk.items():
        form2lemma.setdefault(norm(f), l)
    strongs = json.load(open(args.strongs, encoding="utf-8"))
    lem2g = {}
    for k, e in strongs.items():
        if k.startswith("G") and e.get("lemma"):
            lem2g.setdefault(norm(e["lemma"]), k)

    def to_g(word):
        nf = norm(word)
        if nf in OVERRIDE:
            return OVERRIDE[nf]
        lemma = form2lemma.get(nf)
        if lemma and lem2g.get(norm(lemma)):
            return lem2g[norm(lemma)]
        return lem2g.get(nf)            # the form may itself be a lemma

    words = {}
    for line in open(fetch(SWETE_WORDS, work / "swete-words.csv"), encoding="utf-8"):
        p = line.rstrip("\n").split("\t")
        if len(p) == 2 and p[0].isdigit():
            words[int(p[0])] = p[1]
    vers = []
    for line in open(fetch(SWETE_VERS, work / "swete-vers.csv"), encoding="utf-8"):
        p = line.rstrip("\n").split("\t")
        if len(p) != 2 or not p[0].isdigit() or "." not in p[1] or ":" not in p[1]:
            continue
        bk, cv = p[1].split(".", 1)
        c, v = cv.split(":")
        vers.append((int(p[0]), bk, int(c), int(v)))
    vers.sort()
    maxidx = max(words) if words else 0

    gset = {}
    for i, (idx, bk, c, v) in enumerate(vers):
        kjv = BMAP.get(bk)
        if not kjv:
            continue
        end = vers[i + 1][0] if i + 1 < len(vers) else maxidx + 1
        gs = {g for wi in range(idx, end) if (g := to_g(words.get(wi, "")))}
        if gs:
            gset[(kjv, c, v)] = gs

    hset = {}
    for line in open(args.kjv, encoding="utf-8"):
        if line.startswith('{"format'):
            continue
        o = json.loads(line)
        hs = {s for t in o["t"] for s in t[3] if s.startswith("H")}
        if hs:
            hset[(o["b"], o["c"], o["v"])] = hs

    hc, gc, co = Counter(), Counter(), Counter()
    for r in set(hset) | set(gset):
        for h in hset.get(r, ()):
            hc[h] += 1
        for g in gset.get(r, ()):
            gc[g] += 1
    for r in set(hset) & set(gset):
        for h in hset[r]:
            for g in gset[r]:
                co[(h, g)] += 1

    links = []
    for (h, g), n in co.items():
        dice = 2 * n / (hc[h] + gc[g]) if (hc[h] + gc[g]) else 0.0
        if dice >= args.min_dice and n >= args.min_co:
            links.append({"h": h, "g": g, "source": "lxx", "dice": round(dice, 3), "co": n})
    links.sort(key=lambda x: -x["dice"])

    artifact = {
        "format": "overlay-bridge-sources-v1",
        "source": "lxx",
        "note": "Hebrew<->Greek translation equivalents by Septuagint per-verse "
                "co-occurrence (Dice). Opt-in, off by default. Protocanonical OT "
                "minus Psalms (LXX numbering) and apocrypha (no Hebrew side).",
        "attribution": [
            "Greek text: Swete LXX (1909-1930), public domain",
            "Lemmatizer: cltk/grc_models_cltk, MIT",
            "Hebrew Strong's: tagged KJV (engKJV2006eb), public domain",
            "Strong's numbers: reference scheme (facts)",
        ],
        "links": links,
    }
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    Path(args.out).write_text(json.dumps(artifact, ensure_ascii=False, indent=1) + "\n",
                              encoding="utf-8")
    print(f"wrote {args.out}: {len(links)} links from {len(set(hset) & set(gset))} aligned verses")


if __name__ == "__main__":
    main()
