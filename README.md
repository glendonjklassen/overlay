# overlay

A reader and study tool for the 1769 KJV, with signed point-patches to the
text (Ed25519), classic Strong's word lookup, and concordance
cross-references. No modern translations, no commentary layers.

GUI built with [Monomer](https://github.com/fjvallarino/monomer)
(SDL2 + NanoVG rendering), displayed through WSLg.

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
`.\run.ps1 run overlay-import`, which produces:

- `data/kjv.jsonl` — canonical tokenized text (31,102 verses; tokenization is
  version-stamped and **frozen** — signed patches address into it)
- `data/strongs.json` — merged Hebrew+Greek dictionary (14,197 entries)
- `data/kjv-notes.jsonl` — the 1769 translators' margin notes (kept for a
  future layer, not yet shown)

`.\run.ps1 run overlay '--' --check` verifies the data pipeline headlessly.
(The quotes matter: PowerShell eats a bare `--` before the script sees it.)

## Using the reader

- Book/chapter dropdowns and `<` `>` buttons in the header; `Left`/`Right`
  step chapters and roll across book boundaries.
- Keyboard (the reading pane holds focus; click it if a dropdown stole it):
  `Up`/`Down` scroll by a few lines, `PageUp`/`PageDown`/`Space` by nearly a
  page, `Home`/`End` jump to chapter start/end.
- **Click any word** to open the Strong's panel: the 1890 entry (lemma,
  transliteration, pronunciation, derivation, definition, KJV renderings)
  plus every verse sharing that Strong's number — click an occurrence to jump
  there. `✕` closes.
- Hovering a word underlines it if it carries a Strong's tag.
- **1769 notes** checkbox shows the translators' margin notes beneath their
  verses (the original apparatus — literal Hebrew renderings and variants).
- **patches (N)** opens the patch manager: jump to, or delete, any patch.

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

- **Author one**: right-click a word, or **drag across several words of a
  verse**, then enter the replacement (any number of words) + optional note →
  *Sign & save*. Or from the CLI:
  `.\run.ps1 run overlay '--' --mkpatch Exod 7 7 fourscore eighty`
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

## Development

- `.\run.ps1 test` — unit suite (hspec): tokenizer, signing (incl. a golden
  test freezing the signature byte format), overlap resolution, overlay
  composition.
- `hlint src app tools test` (inside WSL) — kept hint-clean; config in
  [.hlint.yaml](.hlint.yaml).
- `.\run.ps1 run overlay '--' --check` — headless end-to-end check.

Haskell project, built and run inside WSL (Ubuntu). Source lives on the Windows
filesystem; everything compiles and executes on the Linux side.

## Prerequisites

- WSL2 with Ubuntu and the ghcup toolchain (`ghc`, `cabal`) on the PATH.
  Already set up on this machine: GHC 9.6.7, cabal 3.14, stack 3.7 (aarch64).
- System libraries for Monomer (already installed):
  `sudo apt-get install pkg-config libsdl2-dev libglew-dev libgl1-mesa-dev libfreetype-dev fonts-dejavu-core`
- The app currently uses DejaVu Sans from the system font path
  (`/usr/share/fonts/truetype/dejavu/`); bundle a font under `assets/` when
  looks start to matter.

## Build and run (from Windows)

```powershell
.\run.ps1            # cabal run overlay
.\run.ps1 build
.\run.ps1 repl
```

Anything you pass to `run.ps1` is forwarded to `cabal` inside WSL.

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
