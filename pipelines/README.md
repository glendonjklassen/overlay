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
> 1557 links over 20,038 verses). The semantic-domains source was evaluated and
> rejected (see below).

---

## LXX co-occurrence — `lxx_bridge.py` — BUILT

**Why it's trustworthy:** the Septuagint is a translation of the Hebrew, so each
OT verse has the same content in both languages. A Hebrew Strong's number and a
Greek one that keep co-occurring in the same verse are translation equivalents.
Each pair is scored by Dice overlap; one-off coincidences (the "sake/stead"
class) can't clear the floor.

**Full-run result** (protocanonical OT, 20,038 aligned verses): the theological
links etymology + renderings can't see all surface clearly —

| link | dice | co-occur |
|---|---|---|
| H3068 → G2962  (YHWH → *kyrios*) | 0.55 | 2351 |
| H430 → G2316  (elohim → *theos*) | 0.72 | 1599 |
| H4899 → G5547  (Messiah → *Christos*) | 0.66 | 22 |
| H1285 → G1242  (covenant → *diathēkē*) | 0.79 | 206 |
| H7965 → G1515  (shalom → *eirēnē*) | 0.61 | 102 |

Top links by dice are clean proper-noun transliterations (Enoch, Seth, Lamech,
Jephthah…). **Known weakness:** co-occurrence also links a few tightly-bound
collocations (fixed name pairs like *Pedahzur*↔*Gamaliel* that always share a
verse) — contextually related, not strict equivalents; the Dice floor limits it
and word-level alignment would refine it later.

### Reproduce (one command; outputs are yours to license openly)
```sh
python pipelines/lxx_bridge.py        # fetches PD Swete + MIT lemma dict, builds
overlay --check                       # confirms the artifact loads
```
Inputs cache under `pipelines/.lxx-cache/` (gitignored). Provenance: Swete LXX
text (public domain) + cltk lemma dictionary (MIT) + Strong's numbers (facts) +
the tagged KJV Hebrew (public domain) → **no copyleft**, so `bridge/lxx-alignment.json`
is committed and free to relicense.

**Scope / documented exclusions:** the protocanonical OT books whose versification
matches the KJV. Psalms is excluded (LXX numbering differs — needs a mapping) and
the apocrypha / LXX-only books are excluded (no Hebrew counterpart). A
context-free dictionary lemmatizes a few high-frequency divine forms wrongly
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
