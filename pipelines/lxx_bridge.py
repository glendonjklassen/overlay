#!/usr/bin/env python3
"""Build the LXX Hebrew<->Greek bridge artifact by statistical word alignment.

This is the "recipe" behind bridge/lxx-alignment.json — the OPT-IN, off-by-default
cross-testament source. It learns which Hebrew Strong's number the Septuagint
translators rendered with which Greek Strong's number, from the parallel text
itself (the LXX IS a translation of the Hebrew), confirmed by repetition across
the whole corpus. It is ONE corroborating witness, never ground truth, and the
KJV (Textus Receptus + Masoretic) remains the only text the reader treats as
scripture.

INPUTS (you produce these; see pipelines/README.md):
  hebrew.tsv : per Hebrew verse, the content Strong's in order
               <book>\\t<chapter>\\t<verse>\\t H7225 H430 H1254 ...
  greek.tsv  : per Greek (LXX) verse, the content Strong's in order
               <book>\\t<chapter>\\t<verse>\\t G1722 G2316 G4160 ...
  --tvtms (optional): STEPBible versification map, if MT/LXX numbering differs.

LICENSING (all clean for redistribution, hence we COMMIT the output):
  - Hebrew Strong's from Open Scriptures morphhb (WLC), CC-BY 4.0.
  - Greek text: a PUBLIC-DOMAIN LXX (Swete, or Rahlfs-1935). NOT Rahlfs-Hanhart 2006.
  - Versification: STEPBible TVTMS, CC-BY 4.0.
  - Aligner: eflomal (https://github.com/robertostling/eflomal), MIT.
  - Strong's numbers are a reference scheme (facts), so no copyleft attaches.
  The emitted artifact carries these attributions in its header.

OUTPUT: bridge/lxx-alignment.json — {"links":[{"h","g","source":"lxx","count"}...]}.
Verify with:  overlay --analyze   (it reports the bridge link counts + samples).
"""
import argparse
import json
import subprocess
import sys
import tempfile
from collections import Counter
from pathlib import Path


def read_seqs(path):
    """Read a Strong's-sequence TSV into {(book, ch, v): [strongs...]}."""
    out = {}
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            parts = line.rstrip("\n").split("\t")
            if len(parts) < 3:
                continue
            book, ch, vs = parts[0], parts[1], parts[2]
            toks = parts[3].split() if len(parts) > 3 else []
            try:
                out[(book, int(ch), int(vs))] = toks
            except ValueError:
                continue
    return out


def read_tvtms(path):
    """Optional MT->LXX versification remap: {(book,ch,v)_hebrew: (book,ch,v)_greek}.
    Expected TSV columns: hebBook hebCh hebV grkBook grkCh grkV (extra cols ignored)."""
    remap = {}
    with open(path, encoding="utf-8") as fh:
        for line in fh:
            p = line.rstrip("\n").split("\t")
            if len(p) < 6:
                continue
            try:
                remap[(p[0], int(p[1]), int(p[2]))] = (p[3], int(p[4]), int(p[5]))
            except ValueError:
                continue
    return remap


def aligned_verses(heb, grk, remap):
    """Pair Hebrew and Greek verses that both have content tokens."""
    pairs = []
    for ref, htoks in heb.items():
        gref = remap.get(ref, ref)
        gtoks = grk.get(gref)
        if htoks and gtoks:
            pairs.append((htoks, gtoks))
    return pairs


def run_eflomal(pairs, eflomal_bin):
    """Write parallel token files, run eflomal, return forward alignments.
    Each alignment is a list of (hebrew_index, greek_index) pairs (0-based)."""
    with tempfile.TemporaryDirectory() as d:
        src = Path(d) / "heb.txt"
        trg = Path(d) / "grk.txt"
        fwd = Path(d) / "fwd.align"
        src.write_text("\n".join(" ".join(h) for h, _ in pairs) + "\n", encoding="utf-8")
        trg.write_text("\n".join(" ".join(g) for _, g in pairs) + "\n", encoding="utf-8")
        # eflomal-align: -s source -t target -f forward alignments (Pharaoh "i-j" format)
        subprocess.run(
            [eflomal_bin, "-s", str(src), "-t", str(trg), "-f", str(fwd)],
            check=True,
        )
        aligns = []
        for line in fwd.read_text(encoding="utf-8").splitlines():
            cells = []
            for cell in line.split():
                if "-" in cell:
                    i, j = cell.split("-")
                    cells.append((int(i), int(j)))
            aligns.append(cells)
        return aligns


def main():
    ap = argparse.ArgumentParser(description="Build the LXX H<->G bridge artifact.")
    ap.add_argument("--hebrew", required=True, help="Hebrew Strong's-sequence TSV")
    ap.add_argument("--greek", required=True, help="Greek (LXX) Strong's-sequence TSV")
    ap.add_argument("--tvtms", help="optional STEPBible versification TSV")
    ap.add_argument("--out", default="bridge/lxx-alignment.json")
    ap.add_argument("--eflomal-bin", default="eflomal-align")
    ap.add_argument("--min-count", type=int, default=3,
                    help="drop pairs aligned fewer than this many times (noise floor)")
    args = ap.parse_args()

    heb = read_seqs(args.hebrew)
    grk = read_seqs(args.greek)
    remap = read_tvtms(args.tvtms) if args.tvtms else {}
    pairs = aligned_verses(heb, grk, remap)
    if not pairs:
        sys.exit("no aligned verses — check the TSVs and versification map")

    aligns = run_eflomal(pairs, args.eflomal_bin)

    counts = Counter()
    for (htoks, gtoks), cells in zip(pairs, aligns):
        for i, j in cells:
            if i < len(htoks) and j < len(gtoks):
                h, g = htoks[i], gtoks[j]
                if h.startswith("H") and g.startswith("G"):  # only cross-testament
                    counts[(h, g)] += 1

    links = [
        {"h": h, "g": g, "source": "lxx", "count": n}
        for (h, g), n in sorted(counts.items(), key=lambda kv: -kv[1])
        if n >= args.min_count
    ]
    artifact = {
        "format": "overlay-bridge-sources-v1",
        "source": "lxx",
        "note": "Hebrew<->Greek links by Septuagint word-alignment (eflomal). "
                "ONE corroborating witness, not ground truth; opt-in, off by default.",
        "attribution": [
            "Hebrew Strong's: Open Scriptures Hebrew Bible (morphhb), CC-BY 4.0",
            "Versification: STEP Bible TVTMS (www.STEPBible.org), CC-BY 4.0",
            "Greek text: public-domain LXX (Swete / Rahlfs-1935)",
            "Alignment: eflomal (MIT)",
        ],
        "links": links,
    }
    Path(args.out).parent.mkdir(parents=True, exist_ok=True)
    Path(args.out).write_text(json.dumps(artifact, ensure_ascii=False, indent=1) + "\n",
                              encoding="utf-8")
    print(f"wrote {args.out}: {len(links)} links from {len(pairs)} aligned verses "
          f"(min-count {args.min_count})")


if __name__ == "__main__":
    main()
