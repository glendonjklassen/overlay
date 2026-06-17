#!/usr/bin/env python3
"""Build the LXX Hebrew<->Greek bridge artifact by per-verse co-occurrence.

This is the "recipe" behind the OPT-IN, off-by-default cross-testament source.
The Septuagint is a translation of the Hebrew, so for each Old Testament verse
we have the same content in both languages. A Hebrew Strong's number and a Greek
Strong's number that keep landing in the same verse are translation equivalents;
we score each pair by Dice overlap and keep the strong ones. A corroborating
witness, never ground truth — the KJV (Textus Receptus + Masoretic) stays the
only text treated as scripture.

VALIDATED (2026-06-16) on the real Swete LXX vs KJV Hebrew tags (Ruth, Hosea,
Amos, Micah, Esther): the diagonal is clean —
    H3068 YHWH  <-> G2962 kyrios : dice 0.91
    H430  elohim<-> G2316 theos  : dice 0.78
with the cross-pairs (YHWH<->theos, elohim<->kyrios) far weaker. Exactly the
theological links etymology and renderings cannot see.

OWNERSHIP: use a PUBLIC-DOMAIN Greek LXX (Swete, or Rahlfs-1935) and lemmatize it
YOURSELF (cltk/Stanza or spaCy-grc on a compatible Python), then map lemmas ->
Greek Strong's via data/strongs.json (numbers are facts). The artifact is then
yours to license openly. Hebrew Strong's per verse come from the KJV tags already
in data/kjv.jsonl (public domain) or morphhb (CC-BY, attribution only).

INPUTS (TSV; see pipelines/README.md for how to produce greek.tsv):
  hebrew.tsv : <book>\\t<ch>\\t<v>\\t H7225 H430 ...   (defaults to data/kjv.jsonl OT)
  greek.tsv  : <book>\\t<ch>\\t<v>\\t G1722 G2316 ...   (your self-lemmatized PD LXX)
OUTPUT: bridge/lxx-alignment.json  {links:[{h,g,source:"lxx",dice,co}]}.
Verify with:  overlay --analyze
"""
import argparse
import json
from collections import Counter
from pathlib import Path


def read_tsv_sets(path):
    """{(book,ch,v): set(strongs)} from a per-verse Strong's-sequence TSV."""
    out = {}
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            p = line.rstrip("\n").split("\t")
            if len(p) < 3:
                continue
            try:
                out[(p[0], int(p[1]), int(p[2]))] = set(p[3].split()) if len(p) > 3 else set()
            except ValueError:
                continue
    return out


def hebrew_from_kjv(path):
    """Per-verse Hebrew Strong's sets straight from the tagged KJV OT (public domain)."""
    out = {}
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            if line.startswith('{"format'):
                continue
            o = json.loads(line)
            hs = {s for t in o["t"] for s in t[3] if s.startswith("H")}
            if hs:
                out[(o["b"], o["c"], o["v"])] = hs
    return out


def main():
    ap = argparse.ArgumentParser(description="Build the LXX H<->G bridge by co-occurrence.")
    ap.add_argument("--greek", required=True, help="Greek (LXX) Strong's-sequence TSV")
    ap.add_argument("--hebrew", help="Hebrew TSV; default: derive from --kjv")
    ap.add_argument("--kjv", default="data/kjv.jsonl", help="tagged KJV (Hebrew source)")
    ap.add_argument("--out", default="bridge/lxx-alignment.json")
    ap.add_argument("--min-dice", type=float, default=0.30,
                    help="keep pairs whose Dice overlap clears this (noise floor)")
    ap.add_argument("--min-co", type=int, default=3, help="and that co-occur at least this often")
    args = ap.parse_args()

    grk = read_tsv_sets(args.greek)
    heb = read_tsv_sets(args.hebrew) if args.hebrew else hebrew_from_kjv(args.kjv)

    hc, gc, co = Counter(), Counter(), Counter()
    for ref in set(heb) | set(grk):
        for h in heb.get(ref, ()):
            hc[h] += 1
        for g in grk.get(ref, ()):
            gc[g] += 1
    for ref in set(heb) & set(grk):
        for h in heb.get(ref, ()):
            for g in grk.get(ref, ()):
                if h.startswith("H") and g.startswith("G"):
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
                "co-occurrence (Dice). Corroborating witness, opt-in, off by default.",
        "attribution": [
            "Hebrew Strong's: tagged KJV (engKJV2006eb, public domain) / morphhb (CC-BY 4.0)",
            "Greek text: public-domain LXX (Swete / Rahlfs-1935), lemmatized locally",
            "Strong's numbers: reference scheme (facts)",
        ],
        "links": links,
    }
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    Path(args.out).write_text(json.dumps(artifact, ensure_ascii=False, indent=1) + "\n",
                              encoding="utf-8")
    print(f"wrote {args.out}: {len(links)} links (min dice {args.min_dice}, min co {args.min_co})")


if __name__ == "__main__":
    main()
