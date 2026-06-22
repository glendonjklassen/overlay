#!/usr/bin/env python3
"""Build the semantic-domain bridge artifact from the UBS dictionaries.

STATUS: EVALUATED, NOT WIRED INTO THE BUILD. An audit run (see pipelines/README)
showed shared-domain membership is category-level, not concept-level, and misses
the theological links it was meant to provide. Kept as a reproducible record.


OPT-IN, off-by-default cross-testament source. Two lemmas — one Hebrew, one
Greek — that the UBS lexicographers placed in the SAME semantic domain are a
candidate concept link. A corroborating witness, never ground truth.

Sources (both CC-BY-SA 4.0, github.com/ubsicap/ubs-open-license):
  - UBS Greek NT Dictionary (Louw-Nida domains)
  - UBS Dictionary of Biblical Hebrew (SDBH domains)
Because they are CC-BY-**SA**, the derived artifact is share-alike too, so it is
written to gitignored data/ (NEVER committed) and rebuilt locally.

The catch (found by inspecting the data): SDBH and Louw-Nida are different
systems with different English labels. ~40 concrete labels match directly
(Animals, Metals, Food…); the abstract/theological domains do NOT (Hebrew
"Deities" vs Greek "Supernatural Beings"). CROSSWALK below maps the clear
equivalences so e.g. God↔God links. It is a small, COARSE, reviewable
domain-level mapping — not per-pair arbitration. Edit it; don't trust it blindly.

Output: data/bridge-domains.json  (links: {h, g, source:"domain", score, domains}).
"""
import argparse
import json
from collections import defaultdict
from pathlib import Path

# SDBH (Hebrew) domain label  ->  canonical Louw-Nida (Greek) label.
# Keep this SMALL and defensible; every entry is a coarse human judgement.
CROSSWALK = {
    "Deities": "Supernatural Beings",
    "Names of Deities": "Supernatural Beings",
    "Angels": "Supernatural Beings",
    "Demons": "Supernatural Beings",
}


def extract(path):
    """{normalized Strong's -> set(domain labels)} from a UBS dictionary JSON."""
    out = defaultdict(set)
    for e in json.load(open(path, encoding="utf-8")):
        labs = set()
        for bf in e.get("BaseForms") or []:
            for lm in bf.get("LEXMeanings") or []:
                for dom in (lm.get("LEXDomains") or []) + (lm.get("LEXSubDomains") or []):
                    lab = (dom.get("Domain") or "").strip()
                    if lab:
                        labs.add(lab)
        for sc in e.get("StrongCodes") or []:
            if not sc or sc[0] not in "HGA":
                continue
            prefix = "H" if sc[0] == "A" else sc[0]      # Aramaic counts as Hebrew side
            num = sc[1:].lstrip("0") or "0"
            out[prefix + num] |= labs
    return out


def main():
    ap = argparse.ArgumentParser(description="Build the semantic-domain bridge artifact.")
    ap.add_argument("--greek", required=True, help="UBSGreekNTDic JSON")
    ap.add_argument("--hebrew", required=True, help="UBSHebrewDic JSON")
    ap.add_argument("--out", default="data/bridge-domains.json")
    ap.add_argument("--score-floor", type=float, default=0.02,
                    help="drop pairs whose summed domain specificity is below this")
    args = ap.parse_args()

    G = extract(args.greek)
    H = extract(args.hebrew)

    # canonicalize Hebrew labels into the Greek label space via the crosswalk
    def canon(labs):
        return {CROSSWALK.get(l, l) for l in labs}

    # label -> lemma sets, in the shared (canonicalized) label space
    g_by_label = defaultdict(set)
    h_by_label = defaultdict(set)
    for s, labs in G.items():
        for l in canon(labs):
            g_by_label[l].add(s)
    for s, labs in H.items():
        for l in canon(labs):
            h_by_label[l].add(s)

    # score each (H,G) pair by summed domain specificity (specific domains, shared
    # by few lemmas, count for more than broad ones like "Animals")
    pair_score = defaultdict(float)
    pair_domains = defaultdict(set)
    for label in set(g_by_label) & set(h_by_label):
        hs, gs = h_by_label[label], g_by_label[label]
        spec = 1.0 / (len(hs) + len(gs))            # specificity of this domain
        for h in hs:
            for g in gs:
                pair_score[(h, g)] += spec
                pair_domains[(h, g)].add(label)

    links = [
        {"h": h, "g": g, "source": "domain",
         "score": round(sc, 4), "domains": sorted(pair_domains[(h, g)])}
        for (h, g), sc in sorted(pair_score.items(), key=lambda kv: -kv[1])
        if sc >= args.score_floor
    ]
    artifact = {
        "format": "overlay-bridge-sources-v1",
        "source": "domain",
        "note": "Hebrew<->Greek links by shared UBS semantic domain (SDBH + "
                "Louw-Nida), via a small label crosswalk. Corroborating witness, "
                "opt-in, off by default. NOT committed (CC-BY-SA share-alike).",
        "attribution": [
            "UBS Greek NT Dictionary (Louw-Nida), CC-BY-SA 4.0, ubsicap/ubs-open-license",
            "UBS Dictionary of Biblical Hebrew (SDBH), CC-BY-SA 4.0, ubsicap/ubs-open-license",
        ],
        "links": links,
    }
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    Path(args.out).write_text(json.dumps(artifact, ensure_ascii=False, indent=1) + "\n",
                              encoding="utf-8")
    print(f"wrote {args.out}: {len(links)} links (score floor {args.score_floor})")


if __name__ == "__main__":
    main()
