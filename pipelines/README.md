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

> **Status:** the **LXX source is built and committed** (`bridge/lxx-alignment.json`,
> 953 mutually-best IBM Model 1 links over ~22,440 verses, Psalms included). The
> semantic-domains source was evaluated and rejected (see below).

---

## LXX alignment (IBM Model 1) — `lxx_bridge.py` — BUILT

**Why it's trustworthy:** the Septuagint is a translation of the Hebrew, so each
OT verse has the same content in both languages. We run **IBM Model 1** (EM word
alignment, pure Python) in both directions and keep only **mutually-best** pairs —
each side's single best translation of the other. This beats raw co-occurrence:
it suppresses the trap where two words merely share a verse, and one-off
coincidences (the "sake/stead" class) never surface.

**Full-run result** (protocanonical OT **+ Psalms**, ~22,440 aligned verses):
**953 high-precision links.** The theological links etymology + renderings can't
see are all present and correct, and the top links by score are clean
equivalents (Israel, woman↔*gynē*, Jerusalem, water↔*hydōr*, Jacob, daughter,
people↔*laos*, angel↔*angelos*, forty↔*tessarakonta*…):

- H3068 ↔ G2962 (YHWH ↔ *kyrios*), H430 ↔ G2316 (elohim ↔ *theos*)
- H4899 ↔ G5547 (Messiah ↔ *Christos*), H1285 ↔ G1242 (covenant ↔ *diathēkē*)
- H7965 ↔ G1515 (shalom ↔ *eirēnē*), H6666/H6664 ↔ G1343 (righteousness)

**Honest residual:** a handful of *perfectly*-collocated genealogy names (e.g.
*Pedahzur* and *Gamaliel*, which only ever appear in the same census verses)
cannot be told apart by any co-occurrence/alignment statistic — they share
identical contexts. Mutual-best removes the bulk of such noise but can still
attach the wrong member of such a pair. Low-harm (related names, never
theological), and it's why this stays an opt-in, off-by-default *witness*.
Positional alignment (Model 2 / fast_align) could refine these later.

### Reproduce (one command; outputs are yours to license openly)
```sh
python pipelines/lxx_bridge.py        # fetches PD Swete + MIT lemma dict, builds
overlay --check                       # confirms the artifact loads
```
Inputs cache under `pipelines/.lxx-cache/` (gitignored). Provenance: Swete LXX
text (public domain) + cltk lemma dictionary (MIT) + Strong's numbers (facts) +
the tagged KJV Hebrew (public domain) → **no copyleft**, so `bridge/lxx-alignment.json`
is committed and free to relicense.

**Scope / documented exclusions:** the protocanonical OT books, **including Psalms**
(LXX numbering remapped to MT via `ps_lxx_to_mt`; merges/splits approximated, which
Model 1 tolerates). Apocrypha / LXX-only books are excluded (no Hebrew counterpart).
A context-free dictionary lemmatizes a few high-frequency divine forms wrongly
(e.g. θεοῦ→θεάομαι); `OVERRIDE` in the script corrects the θεός paradigm.

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
