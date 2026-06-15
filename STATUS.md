# Status — 2026-06-14

## Working (all verified headlessly; see Needs a hand-test)

- **Data pipeline**: KJV 1769 + classic Strong's tagging → frozen
  tokenization `kjv1769-tok2` (31,102 verses), 1890 Strong's dictionaries,
  1769 margin notes. Importer logic lives in the library
  (`Overlay.Import`) — move verified byte-identical via md5.
- **Reader**: custom widget, per-word layout/hit-testing, owned scrolling,
  Left/Right chapter nav across books, starts maximized, EB Garamond.
- **Strong's**: panel + concordance occurrences, click to jump. Panel body
  now lives in a single vscroll so long entries can't squeeze/overlap the
  caption labels (second attempt — first fix only pinned label heights,
  which then overflowed the panel on entries like H6213).
- **Signed patches**: Ed25519, frozen signing format (golden-tested),
  verify-on-load, overlap resolution, tamper rejection, hover cards,
  right-click and drag-span authoring, patch manager panel with delete.
- **Signed rules** (`Overlay.Rule`, `rules/*.json`, `overlay-rule-v1`):
  corpus-wide rewrites addressed by content — a match word-sequence is
  replaced everywhere it occurs in the canonical tokenization. Same key /
  trust model as patches; frozen signing format (golden-tested). Per-verse
  exclusions are part of the signed content; adding one re-signs but keeps
  `created`, so the rule keeps its precedence slot. Composition: point
  patches always beat rules; earlier-created rule wins overlaps; rules
  match canonical text only (no cascading). Authoring: editor panel scope
  radio ("this verse only" / "everywhere — N matches"), with live corpus
  count; `--mkrule <words...> => <words...>` CLI. Right-clicking a
  rule-rewritten span opens the editor with "exclude this verse from the
  rule" (own rules); patching over a rule span overrides it. Rules are
  listed in the patches panel with status/match/exclusion counts + delete.
- **Threads** (`Overlay.Thread`, `threads/*.json`, `overlay-thread-v1`):
  named trails through the text ("Christ throughout the Bible"). The
  editor panel (right-click word / drag span) has "add span to thread" —
  pick an existing thread or type a new name; the note field travels with
  the entry. Threads panel lists all threads; a thread view shows a
  running notes editor (textArea + save), every passage (snapshot text,
  per-entry note, click-to-jump, remove), and delete-thread. While a
  thread is open its passages get a soft highlight in the reader. Plain
  unsigned JSON (personal study data; doesn't alter rendered text);
  unreadable thread files are reported, never clobbered.
- **Weaves** (`Overlay.Weave`, `weaves/*.json`, `overlay-weave-v2`): parallel
  passages as a **graph of verse-to-verse links** — a weave is a set of
  undirected edges between single verses (carrying a `kind`: retelling / type /
  prophecy / quotation). Transitivity is free (combining A↔B with B↔C joins the
  component); a 2-to-1 correspondence is two edges into one verse; an unmatched
  verse is just an unlinked node, still shown in place. The reader is now
  **1..N panes** (one pane = ordinary reading, the degenerate case; `＋`/`－`
  in the header). Links are an **ambient overlay**: any weave whose two verses
  are both visible across panes draws its connector lines automatically — no
  "open a weave" mode. **Author inline**: click a verse number to select it,
  Shift-click to extend, then `＋ link` joins the selection (zips 1:1 when two
  equal-length selections, else all-to-all) into a weave (extends one the
  verses already belong to, else creates one). The weaves panel browses /
  renames kind / edits notes / removes links / **combines** weaves (the
  transitive merge) / deletes; clicking a weave points the panes at its
  passages. The custom multi-column `ReaderView` draws the lines across pane
  gaps. Plain unsigned JSON; ships five stock examples; v1 grid files migrate
  to v2 on load.
- **1769 margin notes**: toggle in header, rendered beneath verses.
- **Settings**: `~/.config/overlay/config.json` — serif font paths,
  bodySize, lineSpacing. Bundled EB Garamond is the default.
- **Quality**: 57 hspec tests green (`cabal test`); hlint clean
  (`hlint src app tools test`, config in `.hlint.yaml`); `--check` renders
  a rule against the real corpus (unicorn → wild ox over 6 places, since
  removed) alongside the existing patch.

## Needs a hand-test (can't click from here)

- Strong's panel on a long entry (click "did" in Exod 7:7 → H6213):
  derivation / definition / KJV renderings should each be fully readable,
  scrolling the whole panel.
- Rules: right-click a word → "everywhere — N matches" → save; text
  updates corpus-wide; hover card reads "rule · ed25519 verified".
  Right-click the rewritten word → exclude this verse → only that verse
  reverts. Patch over a rule span → patch wins; delete the patch → rule
  reclaims.
- Threads: add a couple of spans to a new thread (note field), open the
  threads panel → thread view: notes save/reload, passages highlight in
  the reader, jump links land on the right chapter, remove entry, delete
  thread.
- Editor panel got taller (scope radios + thread section): check it fits
  at smaller window heights.
- Weaves: 1 pane reads normally (Strong's, patches, drag-to-patch unaffected).
  `＋` to two panes; point one at Exodus 20 and the other at Deuteronomy 5 →
  the stock weave's connector lines appear automatically across the gap, the
  differing Sabbath verse (Exod 20:11 ↔ Deut 5:15) lines up, unmatched verses
  still show. Click verse numbers to select, Shift-click to extend, `＋ link`
  draws new lines (1:1 for equal selections, converging for a 2-to-1). Open a
  stock weave from the list → panes jump to its passages. Combine two weaves →
  their components join. Per-pane scroll, book/chapter nav, and `‹ ›` work.
- Patch manager: delete restores text live; "go" jumps correctly.
- Notes toggle layout at various sizes; note text wrapping.
- EB Garamond rendering quality under llvmpipe (it's a variable font —
  stb_truetype renders the default weight).
- Scroll-wheel direction (one sign flip in ReaderView if backwards).

## Next candidates

- Editing/superseding an existing patch or rule from the UI (today:
  delete + re-add).
- Excluding someone else's rule (today: patch over it; a local unsigned
  override layer would do it without re-signing).
- Search (plain text; maybe Strong's-aware).
- Reorder/annotate thread entries from the UI (today: entries append in
  add order; notes are per-entry at add time).
- Paragraph-mode reading view using the imported ¶ flags.
- Binary corpus cache if the ~3.5s startup parse starts to annoy.
- Hebrew niqqud rendering is crude (nanovg does no OpenType shaping).
