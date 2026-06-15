# overlay

A reader and study tool for the 1769 KJV: classic Strong's word lookup with
concordance cross-references, Ed25519-signed point-patches and corpus-wide
rules layered over the text — **non-destructive overlays; the base KJV text is
never modified** — personal study threads, and weaves that line up parallel
passages. No modern translations, no commentary layers.

GUI built with [Monomer](https://github.com/fjvallarino/monomer)
(SDL2 + NanoVG rendering), displayed through WSLg.

## Disclaimer

> [!IMPORTANT]
> **The only part of this repository that is without error is the Bible, the
> Word of God.** Everything else here is a tool for studying it more closely.
> Code and text in this repo may reflect human or AI inference and are meant
> only to provoke study — never as an addition to, or a removal from, the Word
> (cf. Deuteronomy 4:2; Revelation 22:18–19). Care has been taken to add no
> extrabiblical spin to the text. This is a work in progress, developed with
> AI. Use your own discernment.

## Data

All source data is classic and freely licensed:

- **Text + per-word Strong's tagging**: CrossWire/eBible.org KJV SWORD module
  (`engKJV2006eb`, the 1769 standardized text, public domain), installed via
  `apt install sword-text-kjv libsword-utils` and dumped with
  `mod2imp engKJV2006eb > data/raw/kjv.imp`.
- **Strong's dictionaries** (1890): Open Scriptures JSON conversion
  (CC-BY-SA), from
  [openscriptures/strongs](https://github.com/openscriptures/strongs).

`data/` is gitignored; regenerate it with the two downloads above plus
`.\util\windows_run.ps1 run overlay-import`, which produces:

- `data/kjv.jsonl` — canonical tokenized text (31,102 verses; tokenization is
  version-stamped and **frozen** — signed patches address into it)
- `data/strongs.json` — merged Hebrew+Greek dictionary (14,197 entries)
- `data/kjv-notes.jsonl` — the 1769 translators' margin notes (kept for a
  future layer, not yet shown)

`.\util\windows_run.ps1 run overlay '--' --check` verifies the data pipeline headlessly.
(The quotes matter: PowerShell eats a bare `--` before the script sees it.)

## Using the reader

- Book/chapter dropdowns and `<` `>` buttons in the header; `Left`/`Right`
  step chapters and roll across book boundaries.
- Keyboard (the reading pane holds focus; click it if a dropdown stole it):
  `Up`/`Down` scroll by a few lines, `PageUp`/`PageDown`/`Space` by nearly a
  page, `Home`/`End` jump to chapter start/end. Hold **Shift** while scrolling
  (wheel or `Up`/`Down`) to lock every pane together for line-by-line reading.
- **Ctrl+click any word** to open the Strong's panel: the 1890 entry (lemma,
  transliteration, pronunciation, derivation, definition, KJV renderings)
  plus every verse sharing that Strong's number — click an occurrence to jump
  there. `✕` closes. (Ctrl is required so a stray click while scrolling can't
  swap the side panel out.)
- Hovering a word underlines it if it carries a Strong's tag.
- **1769 notes** checkbox shows the translators' margin notes beneath their
  verses (the original apparatus — literal Hebrew renderings and variants).
- **patches (N)** opens the patch manager: jump to, or delete, any patch.
- A **canon map** runs along the bottom: the whole Bible front to back, banded
  by section (Law · History · Wisdom · Prophets ∣ Gospels · Acts · Letters ·
  Revelation) with the Old/New Testament divide marked and a coloured pin per
  pane — so you can see where, and how far apart, your passages sit.

## Weaves — parallel passages

A **weave** ties parallel passages together: a Gospel harmony, a prophecy against
its fulfillment, a type and its antitype, an OT verse against the NT that quotes
it. Under the hood it is a **graph** — a set of links (edges) between single
verses — tagged with a *kind*: `retelling`, `type`, `prophecy`, or `quotation`.
Because it's a graph, a verse mapping to two (a 2-to-1 correspondence) is just
two converging links, combining two weaves is the union of their links (so A↔B
plus B↔C joins transitively), and a verse with no parallel simply has no link —
it still reads in place.

The reader shows **one to four panes** (one pane is ordinary reading; `＋`/`－`
in the header add and remove panes). Reading two passages side by side is the
normal way to use it.

- **Links are ambient.** Point two panes at parallel passages and any weave that
  connects them — yours or one that shipped — draws its connector lines across
  the gap automatically. There's no weave "mode" to enter.
- **Author inline while reading.** Click a verse number to select it, Shift-click
  another to extend the run, then **`＋ link`** in the header. Equal-length
  selections in two panes link 1:1 (verse 4↔8, 5↔9, …); anything else links
  all-to-all (the 2-to-1 case). The links go into a weave the selected verses
  already belong to, or a new one.
- **weaves (N)** opens the list — browse, set the kind, edit notes, remove
  individual links, **combine** another weave in (the transitive merge), or
  delete. Clicking a weave points the panes at its passages.
- Per pane: book/chapter dropdowns, `‹ ›`, and independent scroll. Ctrl+click
  still opens Strong's; right-click / drag still author patches.
- Each edge may carry a **label** — the exact shared words it points at — shown
  on hover, so a many-to-one parallel (a name-list, say) reads cleanly instead
  of as a tangle of lines.

> [!NOTE]
> The weaves shipped in this repo are **AI-generated study aids, not approved**.
> Every weave records an `approved` flag (all currently `false`) and, where a
> parallel is contested, a `tension` note — both surfaced in the reader. Treat
> them as prompts for study, not as authority, until reviewed.

Weaves are plain unsigned JSON under `weaves/` and never alter the text; older
`overlay-weave-v1` grid files migrate to the graph model on load. The repo ships
a couple dozen examples — from the two creation accounts and the Chronicler's
retelling of David's reign to the Olivet discourse across the synoptics.

## Type and settings

Scripture renders in EB Garamond (OFL, bundled under `assets/fonts/`).
`~/.config/overlay/config.json` (created on first run, inside WSL) overrides
it: `serifRegular`/`serifItalic` take font file paths, `bodySize` and
`lineSpacing` tune the text. Restart to apply.

The reading pane is a custom Monomer widget ([src/Overlay/ReaderView.hs](src/Overlay/ReaderView.hs))
that lays out and hit-tests every word individually — patch markers, hover
cards, and Strong's lookup all ride on the same layout.

## Signed patches

The base text is never modified. A patch replaces a span of words in one
verse, addressed by word index against the frozen tokenization, and is
signed (Ed25519) over a deterministic encoding of its content. Patches live
as JSON files under `patches/`; delete a file to undo its change.

> [!WARNING]
> Patches must never add to, take from, or change the meaning of the text. They
> are strictly for modernizing archaisms — for example `fourscore → eighty`.

- **Author one**: right-click a word, or **drag across several words of a
  verse**, then enter the replacement (any number of words) + optional note →
  *Sign & save*. Or from the CLI:
  `.\util\windows_run.ps1 run overlay '--' --mkpatch Exod 7 7 fourscore eighty`
- **Rendering**: patched words show amber with a dotted underline; hover
  for the original reading, author key fingerprint, date, and verification
  status.
- **Keys**: an Ed25519 keypair is generated on first run into
  `~/.config/overlay/` (inside WSL). Share `ed25519.public` with anyone who
  should trust your patches; list others' public keys (hex, one per line)
  in `~/.config/overlay/trusted-keys`.
- **Verification**: invalid patches (bad signature, edited file, wrong
  tokenization, text mismatch, overlap with an earlier patch) are never
  applied. Valid signatures from unknown keys apply but render orange and
  say so on the card.

## Signed rules

Where a patch affects one verse, a **rule** rewrites a word sequence *everywhere*
it occurs in the canonical text — addressed by content, not position. Like
patches, rules are an Ed25519-signed overlay: verified on load, never touching
the base text. Rules live as JSON under `rules/`.

> [!WARNING]
> Rules must never add to, take from, or change the meaning of the text. Like
> patches, they are strictly for modernizing archaisms — for example
> `fourscore → eighty`.

- **Author one**: right-click a word (or drag a span), choose the scope —
  *this verse only* (a patch) or *everywhere — N matches* (a rule, with a live
  corpus match count) — then *Sign & save*. Or from the CLI:
  `.\util\windows_run.ps1 run overlay '--' --mkrule <words...> => <words...>`
- **Exclusions**: right-click a rule-rewritten span → *exclude this verse from
  the rule* (your own rules only). The exclusion is part of the signed content,
  so it re-signs but keeps the rule's `created` stamp — and thus its precedence.
- **Composition**: point patches always beat rules; among rules, the earlier
  `created` wins where matches overlap; rules match the canonical text only, so
  application never cascades. Patching over a rule span overrides it.
- Rules appear in the **patches (N)** panel with status, match, and exclusion
  counts, plus delete. Same key / trust model as patches.

## Threads

A **thread** is a named trail through the text — *"Christ throughout the
Bible"* — collecting passages (a verse ref plus a word span, down to a single
word), each with an optional note, alongside a running notes document on the
thread itself. Threads are personal study data: plain unsigned JSON under
`threads/`, one file per thread (they never alter the rendered text, so the
patch trust model doesn't apply).

- **Add a passage**: right-click a word / drag a span → *add span to thread* →
  pick an existing thread or name a new one; the note travels with the entry.
- **threads (N)** lists every thread; opening one shows its running notes
  (edit + save), every passage (snapshot text, note, click-to-jump, remove),
  and delete-thread. While a thread is open its passages are softly highlighted
  in the reader.
- Entries snapshot the words they covered when added, so a thread file stays
  readable on its own and survives retokenization gracefully.

## Development

- `.\util\windows_run.ps1 test` — unit suite (hspec): tokenizer, signing (incl. a golden
  test freezing the signature byte format), overlap resolution, overlay
  composition.
- `hlint src app tools test` (inside WSL) — kept hint-clean; config in
  [.hlint.yaml](.hlint.yaml).
- `.\util\windows_run.ps1 run overlay '--' --check` — headless end-to-end check.

Haskell project, built and run inside WSL (Ubuntu). Source lives on the Windows
filesystem; everything compiles and executes on the Linux side.

## Prerequisites

- WSL2 with Ubuntu and the ghcup toolchain (`ghc`, `cabal`) on the PATH.
  Already set up on this machine: GHC 9.6.7, cabal 3.14, stack 3.7 (aarch64).
- System libraries for Monomer (already installed):
  `sudo apt-get install pkg-config libsdl2-dev libglew-dev libgl1-mesa-dev libfreetype-dev`
- Scripture renders in EB Garamond, bundled under `assets/fonts/` (no system
  font needed); see [Type and settings](#type-and-settings) to override it.

## Build and run (from Windows)

```powershell
.\util\windows_run.ps1            # cabal run overlay
.\util\windows_run.ps1 build
.\util\windows_run.ps1 repl
```

Anything you pass to `util\windows_run.ps1` is forwarded to `cabal` inside WSL.

Package metadata lives in `package.yaml` (hpack format); `overlay.cabal` is
generated from it — edit the YAML, never the `.cabal` file. The runner script
regenerates it automatically before every cabal invocation.

Build artifacts go to `~/.cache/overlay-dist` on the Linux filesystem rather
than `./dist-newstyle`, because building across the `/mnt/c` bridge is very
slow. If you run plain `cabal` manually inside WSL, pass
`--builddir="$HOME/.cache/overlay-dist"` to reuse the same build tree.

## IDE support

For HLS-backed editing, open the folder via the VS Code **WSL** extension
(`code .` from a WSL shell) so the Haskell extension runs Linux-side, and
install HLS with `ghcup install hls`. If dependency builds ever feel slow even
with the builddir redirect, the next step is moving the repo itself into the
Linux filesystem (`~/code/overlay`) and accessing it via `\\wsl$`.

## License

Code is [MIT](LICENSE) licensed. The bundled EB Garamond fonts are under the
SIL Open Font License (see [assets/fonts/OFL.txt](assets/fonts/OFL.txt)). The
scripture text and Strong's data are not redistributed here (`data/` is
gitignored); they carry their own licenses — the KJV SWORD module is public
domain, the Open Scriptures Strong's dictionaries are CC-BY-SA (see [Data](#data)).
