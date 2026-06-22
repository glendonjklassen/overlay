#!/usr/bin/env python3
"""Build the LXX Hebrew<->Greek bridge artifact (the OPT-IN cross-testament source).

Self-contained and reproducible. The Septuagint is a translation of the Hebrew,
so each OT verse has the same content in both languages. We learn Hebrew<->Greek
translation equivalents with **IBM Model 1** (EM word alignment, pure Python) and
keep only **mutually-best** pairs — each side's single best translation of the
other. That suppresses the co-occurrence trap (words that merely share a verse,
like the fixed name pair Pedahzur/Gamaliel) while keeping real equivalents.
A corroborating witness, never ground truth.

Full-run result (protocanonical OT + Psalms, ~22,440 aligned verses): ~950
high-precision links, including YHWH<->kyrios, elohim<->theos, Messiah<->Christos,
covenant<->diatheke, shalom<->eirene, righteousness (H6666<->G1343).

Ownership: public-domain Swete Greek text + MIT lemma dictionary + Strong's-number
facts + the public-domain tagged KJV => the artifact is yours to license openly,
and clean to commit.

Sources (cached under --work; fetched once):
  - Swete LXX words + versification: eliranwong/LXX-Swete-1930 (Swete 1909-1930
    text is public domain; we use only the bare words + factual verse boundaries).
  - Greek lemma dictionary: cltk/grc_models_cltk (MIT), form->lemma.
  - Hebrew Strong's per verse: tagged KJV in data/kjv.jsonl (public domain).
  - lemma -> Greek Strong's: data/strongs.json (Strong's numbers are facts).

Scope: protocanonical OT books whose versification matches the KJV, plus Psalms
(LXX numbering remapped to MT). Apocrypha / LXX-only books are excluded (no Hebrew
counterpart). Strong's only covers NT-attested Greek — exactly the Greek we bridge
to; LXX-only words have no G#### and are skipped.
"""
import argparse, json, pickle, unicodedata, urllib.request, math
from collections import Counter, defaultdict
from pathlib import Path

SWETE_WORDS = "https://raw.githubusercontent.com/eliranwong/LXX-Swete-1930/master/02-Swete_word_without_punctuations.csv"
SWETE_VERS  = "https://raw.githubusercontent.com/eliranwong/LXX-Swete-1930/master/00-Swete_versification.csv"
LEMMA_PKL   = "https://raw.githubusercontent.com/cltk/grc_models_cltk/master/lemmata/backoff/greek_lemmata_cltk.pickle"

BMAP = {'Gen':'Gen','Exo':'Exod','Lev':'Lev','Num':'Num','Deu':'Deut','Jos':'Josh',
 'Jdg':'Judg','Rut':'Ruth','1Sa':'1Sam','2Sa':'2Sam','1Ki':'1Kgs','2Ki':'2Kgs',
 '1Ch':'1Chr','2Ch':'2Chr','Ezr':'Ezra','Neh':'Neh','Est':'Esth','Job':'Job',
 'Pro':'Prov','Ecc':'Eccl','Sol':'Song','Isa':'Isa','Jer':'Jer','Lam':'Lam',
 'Eze':'Ezek','Dan':'Dan','Hos':'Hos','Amo':'Amos','Mic':'Mic','Joe':'Joel',
 'Oba':'Obad','Jon':'Jonah','Nah':'Nah','Hab':'Hab','Zep':'Zeph','Hag':'Hag',
 'Zec':'Zech','Mal':'Mal'}
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


def ps_lxx_to_mt(c):
    """Standard LXX->MT Psalm chapter renumbering (dominant offset; merges/splits
    approximated — fine since Model 1 learns corpus-wide and tolerates ±1 noise)."""
    if c <= 8: return c
    if c == 9: return 9
    if 10 <= c <= 112: return c + 1
    if c == 113: return 114
    if c in (114, 115): return 116
    if 116 <= c <= 145: return c + 1
    if c in (146, 147): return 147
    return c


def model1(pairs, iters=5):
    """IBM Model 1 EM: t(target|source). 'source' gets a NULL token."""
    t = defaultdict(lambda: defaultdict(float))
    cooc = defaultdict(set)
    for a, b in pairs:
        for s in set(a) | {'NULL'}:
            cooc[s].update(b)
    for s, ws in cooc.items():
        u = 1.0 / len(ws)
        for w in ws:
            t[s][w] = u
    for _ in range(iters):
        cnt = defaultdict(lambda: defaultdict(float))
        tot = defaultdict(float)
        for a, b in pairs:
            S = list(a) + ['NULL']
            for w in b:
                z = sum(t[s][w] for s in S)
                if z > 0:
                    for s in S:
                        c = t[s][w] / z
                        cnt[s][w] += c
                        tot[s] += c
        for s in cnt:
            for w in cnt[s]:
                t[s][w] = cnt[s][w] / tot[s]
    return t


def topk(t, want, k):
    """For each source, its top-k targets restricted to Strong's prefix `want`."""
    out = {}
    for s, row in t.items():
        ranked = sorted(((w, p) for w, p in row.items() if w.startswith(want)),
                        key=lambda x: -x[1])[:k]
        out[s] = {w for w, _ in ranked}
    return out


def main():
    ap = argparse.ArgumentParser(description="Build the LXX H<->G bridge artifact.")
    ap.add_argument("--work", default="pipelines/.lxx-cache")
    ap.add_argument("--kjv", default="data/kjv.jsonl")
    ap.add_argument("--strongs", default="data/strongs.json")
    ap.add_argument("--out", default="bridge/lxx-alignment.json")
    ap.add_argument("--topk", type=int, default=1, help="mutual best-translation rank (1 = strictest)")
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
        return lem2g.get(nf)

    words = {}
    for line in open(fetch(SWETE_WORDS, work / "swete-words.csv"), encoding="utf-8"):
        p = line.rstrip("\n").split("\t")
        if len(p) == 2 and p[0].isdigit():
            words[int(p[0])] = p[1]
    vers = []
    for line in open(fetch(SWETE_VERS, work / "swete-vers.csv"), encoding="utf-8"):
        p = line.rstrip("\n").split("\t")
        if len(p) == 2 and p[0].isdigit() and "." in p[1] and ":" in p[1]:
            bk, cv = p[1].split(".", 1)
            c, v = cv.split(":")
            vers.append((int(p[0]), bk, int(c), int(v)))
    vers.sort()
    maxidx = max(words) if words else 0

    glist = {}
    for i, (idx, bk, c, v) in enumerate(vers):
        if bk in ("Psa", "Pss"):
            kjv, c = "Ps", ps_lxx_to_mt(c)
        elif bk in BMAP:
            kjv = BMAP[bk]
        else:
            continue
        end = vers[i + 1][0] if i + 1 < len(vers) else maxidx + 1
        gs = [g for wi in range(idx, end) if (g := to_g(words.get(wi, "")))]
        if gs:
            glist.setdefault((kjv, c, v), []).extend(gs)

    hlist = {}
    for line in open(args.kjv, encoding="utf-8"):
        if line.startswith('{"format'):
            continue
        o = json.loads(line)
        hs = [s for t in o["t"] for s in t[3] if s.startswith("H")]
        if hs:
            hlist[(o["b"], o["c"], o["v"])] = hs

    pairs = [(hlist[r], glist[r]) for r in set(hlist) & set(glist)]
    tgh = model1(pairs)
    thg = model1([(b, a) for a, b in pairs])
    topG = topk(tgh, "G", args.topk)   # h -> its best Greek
    topH = topk(thg, "H", args.topk)   # g -> its best Hebrew

    co = Counter()
    for a, b in pairs:
        for h in set(a):
            for g in set(b):
                if h.startswith("H") and g.startswith("G"):
                    co[(h, g)] += 1

    links = []
    for (h, g), n in co.items():
        if n >= args.min_co and g in topG.get(h, ()) and h in topH.get(g, ()):
            sc = round(math.sqrt(tgh[h].get(g, 0) * thg[g].get(h, 0)), 3)
            links.append({"h": h, "g": g, "source": "lxx", "score": sc, "co": n})
    links.sort(key=lambda x: -x["score"])

    artifact = {
        "format": "overlay-bridge-sources-v1",
        "source": "lxx",
        "note": "Hebrew<->Greek translation equivalents from the Septuagint via IBM "
                "Model 1 word alignment, kept where mutually best. Opt-in, off by "
                "default. Protocanonical OT + Psalms (renumbered); apocrypha excluded.",
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
    print(f"wrote {args.out}: {len(links)} mutual-best links from {len(pairs)} aligned verses")


if __name__ == "__main__":
    main()
