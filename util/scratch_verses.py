"""Scratch helper (do not commit): print KJV verse text for a passage.

Usage:
  python scratch_verses.py Gen 1            # whole chapter
  python scratch_verses.py Gen 1 1 3        # verses 1-3
  python scratch_verses.py 2Sam 5 1 10
"""
import json, sys

book = sys.argv[1]
chap = int(sys.argv[2])
v1 = int(sys.argv[3]) if len(sys.argv) > 3 else None
v2 = int(sys.argv[4]) if len(sys.argv) > 4 else v1

with open("data/kjv.jsonl", encoding="utf-8") as f:
    next(f)  # header
    for line in f:
        o = json.loads(line)
        if o["b"] != book or o["c"] != chap:
            continue
        v = o["v"]
        if v1 is not None and not (v1 <= v <= v2):
            continue
        text = "".join(
            (tok[0] or "") + (tok[1] or "") + (tok[2] or "") +
            ("" if i+1 == len(o["t"]) else " ")
            for i, tok in enumerate(o["t"])
        )
        # collapse the trailing spaces a little
        print(f"{book} {chap}:{v}  {text}".rstrip())
