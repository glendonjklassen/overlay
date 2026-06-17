#!/usr/bin/env bash
#
# Hydrate data/ from the freely-licensed source materials, so the corpus never
# has to be reconstructed by hand. Idempotent: each step is skipped when its
# output already exists. Pass --force to rebuild everything from scratch.
#
# Produces (all gitignored, see README "Data"):
#   data/raw/kjv.imp                       <- mod2imp dump of the SWORD module
#   data/raw/strongs-{hebrew,greek}-*.js   <- Open Scriptures Strong's (CC-BY-SA)
#   data/kjv.jsonl, data/strongs.json, data/kjv-notes.jsonl  <- overlay-import
#   data/concept-cache.json                <- overlay --analyze (parallels)
#   data/concept-vectors.vec               <- ml/train_concept2vec.py (embeddings)
#
# Requires: a SWORD install (installmgr, mod2imp), curl, and the Haskell
# toolchain (cabal). On Arch: `sudo pacman -S sword`. On Debian/Ubuntu:
# `sudo apt install libsword-utils sword-text-kjv`. The two derived artifacts
# are optional enrichments: the analysis cache needs only cabal; the embeddings
# need python3 + numpy (skipped with a warning if absent).
#
# Override defaults with env vars, e.g. KJV_MODULE=KJV ./util/hydrate-data.sh
set -euo pipefail

# ── config ───────────────────────────────────────────────────────────────────
KJV_MODULE="${KJV_MODULE:-engKJV2006eb}"          # the SWORD module to dump
KJV_SOURCE="${KJV_SOURCE:-eBible.org}"            # installmgr remote source
STRONGS_BASE="${STRONGS_BASE:-https://raw.githubusercontent.com/openscriptures/strongs/master}"
HEB_JS="data/raw/strongs-hebrew-dictionary.js"
GRK_JS="data/raw/strongs-greek-dictionary.js"
IMP="data/raw/kjv.imp"
OUT="data/kjv.jsonl"

# ── locate repo root (this script lives in util/) ────────────────────────────
ROOT="$(cd "$(dirname "${BASH_SOURCE[0]}")/.." && pwd)"
cd "$ROOT"

FORCE=0
[[ "${1:-}" == "--force" ]] && FORCE=1

log()  { printf '\033[1;36m==>\033[0m %s\n' "$*"; }
warn() { printf '\033[1;33m warn:\033[0m %s\n' "$*" >&2; }
die()  { printf '\033[1;31merror:\033[0m %s\n' "$*" >&2; exit 1; }

need() { command -v "$1" >/dev/null 2>&1 || die "missing '$1' — see the header of this script for install instructions."; }

mkdir -p data/raw

if (( FORCE )); then
    log "force: removing generated data"
    rm -f "$OUT" data/strongs.json data/kjv-notes.jsonl "$IMP" "$HEB_JS" "$GRK_JS" \
          data/concept-cache.json data/concept-vectors.vec
fi

# ── already done? ────────────────────────────────────────────────────────────
# The corpus may already be hydrated; if so we skip steps 1-3 entirely (no SWORD
# or network needed) and fall through to the optional derived data in 4-5.
NEED_CORPUS=1
if [[ -s "$OUT" && -s data/strongs.json ]]; then
    log "corpus present; building any missing derived data. Use --force to rebuild all."
    NEED_CORPUS=0
fi

# ── 1. SWORD module -> data/raw/kjv.imp ──────────────────────────────────────
if (( NEED_CORPUS )) && [[ ! -s "$IMP" ]]; then
    need installmgr
    need mod2imp

    # Install destination must be writable. The system DataPath (often
    # /usr/share/sword) is root-owned, so fall back to ~/.sword for the user.
    if [[ ! -f "$HOME/.sword/sword.conf" ]] && ! mod2imp "$KJV_MODULE" >/dev/null 2>&1; then
        log "pointing SWORD at a user-writable path (~/.sword)"
        mkdir -p "$HOME/.sword/mods.d" "$HOME/.sword/modules"
        printf '[Install]\nDataPath=%s/\n' "$HOME/.sword" > "$HOME/.sword/sword.conf"
    fi

    if ! installmgr -l 2>/dev/null | grep -q "$KJV_MODULE"; then
        log "installing SWORD module '$KJV_MODULE' from $KJV_SOURCE"
        # installmgr prompts a disclaimer to enable remote sources; feed it "yes".
        installmgr -init                       >/dev/null 2>&1 || true
        printf 'yes\n' | installmgr -sc        >/dev/null
        printf 'yes\n' | installmgr -r  "$KJV_SOURCE" >/dev/null
        printf 'yes\n' | installmgr -ri "$KJV_SOURCE" "$KJV_MODULE" >/dev/null \
            || die "installmgr could not install $KJV_MODULE from $KJV_SOURCE"
    fi

    log "dumping '$KJV_MODULE' -> $IMP"
    mod2imp "$KJV_MODULE" > "$IMP"
    [[ -s "$IMP" ]] || die "mod2imp produced an empty $IMP"
else
    log "$IMP present, skipping SWORD dump"
fi

# ── 2. Open Scriptures Strong's dictionaries -> data/raw/*.js ────────────────
fetch() { # url dest
    [[ -s "$2" ]] && { log "$(basename "$2") present, skipping download"; return; }
    need curl
    log "downloading $(basename "$2")"
    curl -fsSL "$1" -o "$2" || die "failed to download $1"
}
if (( NEED_CORPUS )); then
    fetch "$STRONGS_BASE/hebrew/strongs-hebrew-dictionary.js" "$HEB_JS"
    fetch "$STRONGS_BASE/greek/strongs-greek-dictionary.js"   "$GRK_JS"
fi

# ── 3. build the canonical data files ────────────────────────────────────────
if [[ ! -s "$OUT" ]]; then
    need cabal
    log "running overlay-import"
    cabal run -v0 overlay-import
    [[ -s "$OUT" ]] || die "overlay-import did not produce $OUT"
else
    log "$OUT present, skipping overlay-import"
fi

# ── 4. concept-analysis cache (optional) -> data/concept-cache.json ──────────
# Heavy within-language parallel detection; powers the "parallels" review tab.
if [[ ! -s data/concept-cache.json ]]; then
    need cabal
    log "building concept-analysis cache (overlay --analyze)"
    cabal run -v0 overlay -- --analyze \
        || warn "analyze failed; the parallels tab will be empty until it succeeds"
else
    log "data/concept-cache.json present, skipping analyze"
fi

# ── 5. concept embeddings (optional) -> data/concept-vectors.vec ─────────────
# Trained only on the KJV + its Strong's tags (owned, era-faithful); powers the
# "concepts near this" / "verses like this" panel sections.
if [[ ! -s data/concept-vectors.vec ]]; then
    if command -v python3 >/dev/null 2>&1 && python3 -c 'import numpy' 2>/dev/null; then
        log "training concept2vec embeddings (ml/train_concept2vec.py)"
        python3 ml/train_concept2vec.py \
            || warn "embedding training failed; semantic neighbours stay hidden"
    else
        warn "python3 + numpy not found — skipping concept embeddings (optional)."
        warn "  install them, then run: python3 ml/train_concept2vec.py"
    fi
else
    log "data/concept-vectors.vec present, skipping embedding training"
fi

log "done. data/ is hydrated."
