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

## LXX alignment — `lxx_bridge.py`

**Why it's trustworthy:** the Septuagint is a translation of the Hebrew, so for
~23,000 verses we have the same text in both languages. eflomal learns which
Hebrew word maps to which Greek word from co-occurrence across the *whole*
corpus; a pairing only earns weight by recurring (one-off coincidences like the
"sake/stead" rendering glitches can't survive). Output is count-weighted and
floored, then fused as one low-trust witness.

### Prerequisites
```sh
pip install eflomal            # MIT statistical word aligner (CPU; not LLM-tier)
```

### Step 1 — Hebrew sequences (`hebrew.tsv`)
Source: **Open Scriptures morphhb** (WLC), CC-BY 4.0 — `github.com/openscriptures/morphhb`.
Parse the OSIS XML; for each verse emit the content lemmas' Strong's in order,
dropping the clitic prefixes/suffixes morphhb already separates:
```
Gen	1	1	H7225 H430 H1254 H8064 H776
```
(`<w lemma="b/7225 a">` → take the digits → `H7225`; skip conjunction/article/
preposition particles.)

### Step 2 — Greek sequences (`greek.tsv`) — the hard part, be honest
You need a **public-domain** Greek LXX (Swete, or Rahlfs-1935 — **not**
Rahlfs-Hanhart 2006) with each content word mapped to a Greek Strong's:
```
Gen	1	1	G1722 G746 G4160 G2316 G3772 G1093
```
There is no clean FOSS Strong's-tagged LXX (see the research notes), so you
produce it one of two ways:
- **Lemmatize + map:** tokenize the PD Greek text, lemmatize (e.g. CLTK), map
  each lemma → Greek Strong's via `data/strongs.json`. Cleanest license, most
  work.
- **Export a tagged module** that carries Greek Strong's (e.g. the SWORD LXX
  module uses `WG####`) — quick, but note its text lineage/licensing before you
  redistribute anything derived from it.
Drop articles/particles; keep content words in reading order.

### Step 3 — align + emit
```sh
python pipelines/lxx_bridge.py \
    --hebrew hebrew.tsv --greek greek.tsv \
    --tvtms tvtms.tsv \            # optional: STEPBible versification (Psalms etc.)
    --min-count 3 \
    --out bridge/lxx-alignment.json
```
`--tvtms` reconciles MT-vs-LXX verse numbering (STEPBible TVTMS, CC-BY 4.0);
omit it if your inputs already share numbering.

### Step 4 — validate, then commit
```sh
overlay --analyze        # reports bridge link counts + sample pairs
```
Spot-check that the obvious links are present and sane — e.g. **H3068→G2962**
(YHWH→Lord), **H4899→G5547** (Messiah→Christ), **H430→G2316** (God→God) — and
that nothing absurd tops the list. Then `git add bridge/lxx-alignment.json`.

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
