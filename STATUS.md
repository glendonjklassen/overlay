# Status — 2026-06-10

## Working (all verified headlessly; see Needs a hand-test)

- **Data pipeline**: KJV 1769 + classic Strong's tagging → frozen
  tokenization `kjv1769-tok2` (31,102 verses), 1890 Strong's dictionaries,
  1769 margin notes. Importer logic lives in the library
  (`Overlay.Import`) — move verified byte-identical via md5.
- **Reader**: custom widget, per-word layout/hit-testing, owned scrolling,
  Left/Right chapter nav across books, starts maximized, EB Garamond.
- **Strong's**: panel + concordance occurrences, click to jump.
- **Signed patches**: Ed25519, frozen signing format (golden-tested),
  verify-on-load, overlap resolution, tamper rejection, hover cards,
  right-click and drag-span authoring, patch manager panel with delete.
- **1769 margin notes**: toggle in header, rendered beneath verses.
- **Settings**: `~/.config/overlay/config.json` — serif font paths,
  bodySize, lineSpacing. Bundled EB Garamond is the default.
- **Quality**: 26 hspec tests green (`cabal test`); hlint clean
  (`hlint src app tools test`, config in `.hlint.yaml`).

## Needs a hand-test (can't click from here)

- Drag across words → editor opens with the span; replacement saves and
  renders. Edge: drag that leaves the window.
- Patch manager: delete restores text live; "go" jumps correctly.
- Notes toggle layout at various sizes; note text wrapping.
- EB Garamond rendering quality under llvmpipe (it's a variable font —
  stb_truetype renders the default weight).
- Scroll-wheel direction (one sign flip in ReaderView if backwards).

## Next candidates

- Editing/superseding an existing patch from the UI (today: delete + re-add).
- Search (plain text; maybe Strong's-aware).
- Paragraph-mode reading view using the imported ¶ flags.
- Personal notes layer (same addressing as patches).
- Binary corpus cache if the ~3.5s startup parse starts to annoy.
- Hebrew niqqud rendering is crude (nanovg does no OpenType shaping).
