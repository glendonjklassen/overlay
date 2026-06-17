# Bridge source pipelines (opt-in, off by default)

These build the **optional** external sources for the OT↔NT concept bridge. They
are *corroborating witnesses*, never ground truth — the KJV (Textus Receptus +
Masoretic) stays the only text treated as scripture. The app hides these sources
unless you tick **Options → "cross-testament extras (LXX, domains)"**.

Each source emits the shared format the app loads:

```json
{ "format": "overlay-bridge-sources-v1",
  "source": "lxx",
  "attribution": [ "..." ],
  "links": [ { "h": "H4899", "g": "G5547", "source": "lxx", "count": 142 }, ... ] }
```

- **LXX** → `bridge/lxx-alignment.json` — **committed** (CC-BY + public-domain
  inputs, MIT tool, Strong's numbers as facts → clean to redistribute with the
  attributions the artifact carries).
- **Semantic domains** → `data/bridge-domains.json` — **gitignored/hydrated**
  (Louw-Nida/SDBH are CC-BY-**SA** copyleft, so the derived artifact isn't
  committed; rebuild locally).

> ⚠️ **Status:** v1, written but **not yet run end-to-end** — it needs your
> machine (eflomal + the texts). Expect to iterate, especially the Greek tagging
> (step 2). Validate any output with `overlay --analyze` before committing it.

---

## LXX co-occurrence — `lxx_bridge.py` — VALIDATED

**Why it's trustworthy:** the Septuagint is a translation of the Hebrew, so each
OT verse has the same content in both languages. A Hebrew Strong's number and a
Greek one that keep co-occurring in the same verse are translation equivalents.
We score each pair by Dice overlap and keep the strong ones; one-off
coincidences (the "sake/stead" class) can't clear the floor.

**Validated 2026-06-16** on the real Swete LXX vs the KJV Hebrew tags (Ruth,
Hosea, Amos, Micah, Esther — the books then available):

| | G2316 *theos*/God | G2962 *kyrios*/Lord |
|---|---|---|
| **H3068 YHWH** | dice 0.29 | **dice 0.91** |
| **H430 elohim** | **dice 0.78** | dice 0.27 |

The diagonal dominates — exactly the theological links etymology + renderings
miss. (No aligner needed; per-verse co-occurrence + Dice is enough and lighter
than eflomal.)

### Ownership
Use a **public-domain** Greek LXX (Swete, or Rahlfs-1935 — **not** Rahlfs-Hanhart
2006) and **lemmatize it yourself**, so the artifact is yours to license openly.
Hebrew comes from the tagged KJV already in `data/kjv.jsonl` (public domain), so
the only external input is the PD Greek text + your lemmatizer.

### Step 1 — Greek sequences (`greek.tsv`): self-lemmatize the PD LXX
Tokenize the PD Greek text, lemmatize (cltk≥2 via Stanza, or spaCy-grc — needs a
Python the model supports, ≤3.11), and map each lemma → Greek Strong's via
`data/strongs.json` (verified: θεός→G2316, κύριος→G2962, χριστός→G5547, 5495
lemmas). Emit one verse per line:
```
Ruth	1	1	G2962 G1096 G2424 ...
```
Mapping note: Strong's only covers NT-attested Greek, which is exactly the Greek
we want to bridge to; LXX-only words simply have no `G####` and are skipped.

### Step 2 — build + emit  (Hebrew defaults to the tagged KJV)
```sh
python pipelines/lxx_bridge.py --greek greek.tsv \
    --min-dice 0.30 --min-co 3 --out bridge/lxx-alignment.json
```

### Step 3 — validate, then commit
```sh
overlay --analyze        # reports bridge link counts + sample pairs
```
Spot-check the obvious links (H3068→G2962, H430→G2316) top the list and nothing
absurd does. Then `git add bridge/lxx-alignment.json` — clean to commit, since
PD text + your lemmatization + Strong's-number facts impose no copyleft.

---

## Semantic domains — `domains_bridge.py` — EVALUATED, NOT SHIPPED
Louw-Nida (Greek, from UBS) + SDBH (Hebrew, from UBS), both CC-BY-SA via
[`ubsicap/ubs-open-license`](https://github.com/ubsicap/ubs-open-license). The
idea: two Strong's in the same semantic domain → a candidate link.

**Verdict (built + audited 2026-06-16): too coarse to ship.** Running it on the
real UBS data produced ~3,175 links, but:
- They are **category-level, not concept-level** — broad domains (Metals 496,
  Praise 493, Hear 448, Musical Instruments 360) cross-product every Hebrew
  member with every Greek member ("same category", not "same concept").
- The **theological links we wanted are absent** (`H430↔G2316` God, `H3068↔G2962`
  Lord, `H4899↔G5547` Messiah↔Christ): they sit in broad domains ("Supernatural
  Beings") whose per-pair specificity falls below any sane noise floor, even with
  the Deities→Supernatural-Beings crosswalk.
- Root cause: domain co-membership can't distinguish "same concept across
  languages" from "same broad category", so it is not a clean bridge.

The script is kept as a reproducible record of the investigation; it is **not
wired into the build** and emits nothing into the repo. The theological links it
was meant to provide come from the **LXX alignment** above (the LXX renders
mashiach as christos, YHWH as kyrios — actual translation equivalents), which is
the right vehicle. SDBH↔Louw-Nida is CC-BY-SA, so any future artifact would be
gitignored `data/` only.
