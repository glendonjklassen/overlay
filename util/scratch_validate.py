import json, glob, sys, re

# build set of valid (book,chap,verse)
valid=set()
with open("data/kjv.jsonl",encoding="utf-8") as f:
    next(f)
    for line in f:
        o=json.loads(line); valid.add((o["b"],o["c"],o["v"]))

def parse(ref):
    m=re.match(r"^(\S+)\s+(\d+):(\d+)$",ref)
    if not m: return None
    return (m.group(1),int(m.group(2)),int(m.group(3)))

bad=0
for path in sorted(glob.glob("weaves/*.json")):
    d=json.load(open(path,encoding="utf-8"))
    n=len(d.get("links",[]))
    errs=[]
    for l in d.get("links",[]):
        for side in ("a","b"):
            r=parse(l[side])
            if r is None: errs.append(f"unparseable {l[side]}")
            elif r not in valid: errs.append(f"missing verse {l[side]}")
    status="OK" if not errs else "ERR"
    print(f"[{status}] {path}: {n} links")
    for e in errs[:10]:
        print("      -",e); bad+=1
sys.exit(1 if bad else 0)
