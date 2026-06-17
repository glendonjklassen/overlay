{-# LANGUAGE OverloadedStrings #-}

module Overlay.Startup where

import Data.Aeson (Value, decodeFileStrict, encodeFile, object, (.=))
import Data.List (findIndex, sortBy)
import qualified Data.Map.Strict as M
import Data.Ord (Down (..), comparing)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V
import Monomer
import System.Directory (doesFileExist)
import System.Exit (die)

import Overlay.Bridge
import Overlay.Concept
import Overlay.Config
import Overlay.Corpus
import Overlay.Events
import Overlay.Patch
import Overlay.ReaderView
import Overlay.Refs
import Overlay.Render
import Overlay.Rule
import Overlay.Session
import Overlay.Strongs
import Overlay.Tasks
import Overlay.Thread
import Overlay.Types
import Overlay.UI
import Overlay.Weave

corpusPath, strongsPath, conceptCachePath :: FilePath
corpusPath = "data/kjv.jsonl"
strongsPath = "data/strongs.json"
conceptCachePath = "data/concept-cache.json"  -- ^ written by --analyze (gitignored)

-- ── startup ─────────────────────────────────────────────────────────────────

loadEnv :: IO Env
loadEnv = do
    corpus <- loadCorpus corpusPath >>= either dieLoad pure
    strongs <- loadStrongs strongsPath >>= either dieLoad pure
    keys <- loadOrCreateKeys >>= either (die . ("key setup failed: " <>)) pure
    notes <- loadNotes
    extraIx <- sourceLinkIndex <$> loadBridgeSources
    let bridge = etymologyBridge strongs            -- static; approvals live in the model
        candIx = candidateIndex (renderingCandidates corpus)
    Env corpus strongs (occurrenceIndex corpus) (buildConceptIx corpus) bridge candIx
        extraIx keys notes <$> loadSettings
  where
    dieLoad err = die $
        "could not load data: " <> err
        <> "\nrun the importer first: .\\run.ps1 run overlay-import"

guiMain :: IO ()
guiMain = do
    env <- loadEnv
    patches <- loadPatches (envKeys env) (envCorpus env)
    rules <- loadRules (envKeys env) (envCorpus env)
    (threads, terrs) <- loadThreads
    (weaves, werrs) <- loadWeaves
    (serifR, serifI) <- resolveFonts (envSettings env)
    (sansR, sansB) <- resolveSans
    session <- loadSession
    bridgeStore <- loadApprovals bridgeFile
    let maxCols = clampMaxCols (sessMaxCols session)
        panes = restorePanes (envCorpus env) maxCols (sessPanes session)
        status = T.intercalate " · " (filter (not . T.null)
            [ if null terrs then "" else threadErrText terrs
            , if null werrs then "" else weaveErrText werrs ])
        model = AppModel PNone False False True patches rules threads "" "" False "" "" ""
            status panes Nothing weaves "" Retelling Retelling "" "" Nothing
            (sBodySize (envSettings env)) maxCols 0 (sLineSpacing (envSettings env)) []
            bridgeStore False
        config =
            [ appWindowTitle "overlay — KJV 1769"
            , appWindowState MainWindowMaximized
            , appTheme darkTheme
            , appFontDef "Regular" sansR
            , appFontDef "Bold" sansB
            , appFontDef "Serif" serifR
            , appFontDef "Serif Italic" serifI
            , appInitEvent EvInit
            , appDisposeEvent EvSaveSession
            ]
    startApp model (handleEvent env) (buildUI env) config

-- | Headless sanity check of the data pipeline and overlays (--check).
checkMain :: IO ()
checkMain = do
    env <- loadEnv
    let corpus = envCorpus env
        vs = cVerses corpus
        nTokens = V.sum (V.map (length . vTokens) vs)
        render bid c v = case M.lookup (bid, c, v) (cByRef corpus) of
            Nothing -> "MISSING " <> bid <> " " <> showt c <> ":" <> showt v
            Just i ->
                let vr = vs V.! i
                    title = verseTitle vr
                in (if T.null title then "" else "[" <> title <> "] ")
                    <> verseBody vr
        lordOccs = M.findWithDefault [] "H3068" (envOccIx env)
        cix = envConcept env
    cacheLine <- do
        present <- doesFileExist conceptCachePath
        if not present
            then pure "concept:  cache absent (run --analyze to build it)"
            else do
                parsed <- decodeFileStrict conceptCachePath :: IO (Maybe Value)
                pure $ case parsed of
                    Just _  -> "concept:  cache present & parses"
                    Nothing -> "concept:  cache present but UNPARSEABLE"
    st <- selfTest (envKeys env) (cTokVersion corpus)
    patches <- loadPatches (envKeys env) corpus
    rules <- loadRules (envKeys env) corpus
    (threads, terrs) <- loadThreads
    (weaves, werrs) <- loadWeaves
    (serifR, serifI) <- resolveFonts (envSettings env)
    let applied status = case status of
            PInvalid _ -> False
            _ -> True
        isApplied = applied . lpStatus
        renderVerse v = T.unwords (map (renderToken . rtTok) (rvTokens
            (toRVerse (kPubHex (envKeys env)) patches rules [] [] 0 v)))
        renderRef ref = case M.lookup ref (cByRef corpus) of
            Nothing -> "?"
            Just i -> renderVerse (vs V.! i)
        linkRender (Link a b lbl _) =
            refKey a <> " ↔ " <> refKey b
                <> (if T.null lbl then "" else " (" <> lbl <> ")")
                <> " — “"
                <> T.take 28 (renderRef a) <> "…” ↔ “"
                <> T.take 28 (renderRef b) <> "…”"
        allLinks = [l | lw <- weaves, l <- wLinks (lwWeave lw)]
        patchedRender lp =
            let p = lpPatch lp
                ref = (pBook p, pChapter p, pVerse p)
            in case M.lookup ref (cByRef corpus) of
                Nothing -> "  (missing verse)"
                Just i -> "  " <> refText ref <> " → " <> renderVerse (vs V.! i)
        ruleRender lr =
            case V.find (not . null . ruleHitsForVerse [lr] []) vs of
                Nothing -> "  (no matches)"
                Just v -> "  " <> refText (vBook v, vChapter v, vVerse v)
                    <> " → " <> renderVerse v
    putStrLn $ T.unpack $ T.unlines $
        [ "verses:   " <> showt (V.length vs)
        , "books:    " <> showt (M.size (cChapters corpus))
        , "tokens:   " <> showt nTokens
        , "tokver:   " <> cTokVersion corpus
        , "strongs:  " <> showt (M.size (envStrongs env))
        , "concepts: " <> showt (M.size cix) <> " Strong's numbers tagged, "
            <> showt (length (hapaxes cix)) <> " hapax"
        , "bridge:   " <> showt (bridgeSize (envBridge env))
            <> " linked Strong's numbers (etymology + approvals)"
        , cacheLine
        , "notes:    " <> showt (sum (map length (M.elems (envNotes env))))
            <> " margin notes on " <> showt (M.size (envNotes env)) <> " verses"
        , "fonts:    " <> serifR <> " / " <> serifI
        , "type:     " <> showt (sBodySize (envSettings env)) <> "px, spacing "
            <> showt (sLineSpacing (envSettings env))
        , "H3068 in: " <> showt (length lordOccs) <> " verses, first: "
            <> T.intercalate "; " (map refLabel (take 3 lordOccs))
        , "key:      " <> fingerprint (kPubHex (envKeys env)) <> "… ("
            <> showt (T.length (kPubHex (envKeys env))) <> " hex chars)"
        , "ed25519:  " <> either T.pack (const "sign/verify roundtrip OK") st
        , "patches:  " <> showt (length patches) <> " loaded"
        ]
        <> [ "  " <> T.pack (lpFile lp) <> " — " <> showt (lpStatus lp)
           | lp <- patches ]
        <> [ patchedRender lp | lp <- patches, isApplied lp ]
        <> [ "rules:    " <> showt (length rules) <> " loaded" ]
        <> [ "  " <> T.pack (lrFile lr) <> " — " <> showt (lrStatus lr)
             <> " · " <> showt (lrMatches lr) <> " places"
           | lr <- rules ]
        <> [ ruleRender lr | lr <- rules, applied (lrStatus lr) ]
        <> [ "threads:  " <> showt (length threads) <> " ("
             <> showt (sum [length (thEntries (ltThread lt)) | lt <- threads])
             <> " passages)"
             <> (if null terrs then "" else " · " <> threadErrText terrs) ]
        <> [ "weaves:   " <> showt (length weaves) <> " ("
             <> showt (length allLinks) <> " links, "
             <> showt (sum [length (components (wLinks (lwWeave lw)))
                           | lw <- weaves]) <> " groups)"
             <> (if null werrs then "" else " · " <> weaveErrText werrs) ]
        <> [ "  " <> wName (lwWeave lw) <> " — " <> kindLabel (wKind (lwWeave lw))
             <> " · " <> showt (length (wLinks (lwWeave lw))) <> " links"
           | lw <- weaves ]
        <> [ "  " <> linkRender l | l <- take 1 allLinks ]
        <> [ ""
           , "Gen 1:1   " <> render "Gen" 1 1
           , "Ps 3:1    " <> render "Ps" 3 1
           , "John 11:35  " <> render "John" 11 35
           ]

-- | Build the heavy concept indices, cache them to disk, and print a report
-- (--analyze). The light per-concept stats already live in 'envConcept'; this
-- adds the full co-occurrence matrix and the within-language shared-lemma-run
-- candidates, written to a gitignored cache for later phases to load.
analyzeMain :: IO ()
analyzeMain = do
    env <- loadEnv
    let corpus = envCorpus env
        cix = envConcept env
        co = coOccurrence corpus
        coSorted = sortBy (comparing (Down . snd)) (M.toList co)
        cands = quotationCandidates defaultMinRun corpus
        topCoN = 20000 :: Int
        topCandN = 2000 :: Int
        topCo = take topCoN coSorted
        topCand = take topCandN cands
        -- OT↔NT bridge: Strong's etymology (auto) + 1769-rendering candidates
        ety = etymologyLinks (envStrongs env)
        rends = renderingCandidates corpus
        topRend = take topCandN rends
        cacheVal = object
            [ "format" .= ("overlay-concept-cache-v1" :: Text)
            , "tokenization" .= cTokVersion corpus
            , "topCooccur" .=
                [ object ["a" .= a, "b" .= b, "n" .= n] | ((a, b), n) <- topCo ]
            , "candidates" .=
                [ object [ "a" .= refKey (qcA c), "b" .= refKey (qcB c)
                         , "run" .= qcRun c, "len" .= qcLen c ]
                | c <- topCand ]
            , "bridgeEtymology" .=
                [ object ["h" .= blHeb l, "g" .= blGrk l] | l <- ety ]
            , "bridgeRenderCandidates" .=
                [ object [ "h" .= rcHeb c, "g" .= rcGrk c
                         , "word" .= rcWord c, "score" .= rcScore c ]
                | c <- topRend ]
            ]
    encodeFile conceptCachePath cacheVal
    putStrLn $ T.unpack $ T.unlines $
        [ "concepts:   " <> showt (M.size cix) <> " Strong's numbers tagged"
        , "hapax:      " <> showt (length (hapaxes cix))
        , "co-occur:   " <> showt (M.size co) <> " distinct pairs ("
            <> showt (length topCo) <> " cached)"
        , "candidates: " <> showt (length cands)
            <> " within-language parallels (run ≥ " <> showt defaultMinRun
            <> "; " <> showt (length topCand) <> " cached)"
        , "wrote:      " <> T.pack conceptCachePath
        , ""
        , "top co-occurring lemmas (share a verse):"
        ]
        <> [ "  " <> a <> " + " <> b <> "  ×" <> showt n
           | ((a, b), n) <- take 12 coSorted ]
        <> [ "", "longest within-language shared-lemma runs:" ]
        <> [ "  " <> refKey (qcA c) <> " ↔ " <> refKey (qcB c)
             <> "  (" <> showt (qcLen c) <> " lemmas)"
           | c <- take 12 cands ]
        <> [ ""
           , "bridge:     " <> showt (length ety) <> " etymology links, "
               <> showt (length rends) <> " rendering candidates ("
               <> showt (length topRend) <> " cached)"
           , ""
           , "top OT↔NT rendering-bridge candidates (1769 equivalences):" ]
        <> [ "  " <> rcHeb c <> " ↔ " <> rcGrk c <> "  “" <> rcWord c <> "”"
           | c <- take 12 rends ]

-- | Dev helper: create a signed patch from the command line.
-- Usage: overlay --mkpatch <book> <chapter> <verse> <original-word> <replacement...>
mkPatchCli :: [String] -> IO ()
mkPatchCli args = case args of
    (b : c : v : orig : replWords@(_ : _)) -> do
        env <- loadEnv
        let corpus = envCorpus env
            bid = T.pack b
            ref = (bid, read c, read v)
            origT = T.pack orig
        i <- maybe (die "no such verse") pure (M.lookup ref (cByRef corpus))
        let toks = vTokens (cVerses corpus V.! i)
        wordIx <- maybe (die ("word not found; verse reads: "
                <> T.unpack (T.unwords (map tokWord toks)))) pure
            (findIndex ((== origT) . tokWord) toks)
        p <- mkPatch (envKeys env) (cTokVersion corpus) ref (wordIx, wordIx)
            [origT] (map T.pack replWords) Nothing
        path <- savePatch p
        putStrLn ("wrote " <> path)
    _ -> die "usage: overlay --mkpatch <book> <ch> <v> <original> <replacement...>"

-- | Dev helper: create a signed corpus-wide rule from the command line.
-- Usage: overlay --mkrule <match-words...> => <replacement-words...>
mkRuleCli :: [String] -> IO ()
mkRuleCli args = case break (== "=>") args of
    (match@(_ : _), _ : repl@(_ : _)) -> do
        env <- loadEnv
        let corpus = envCorpus env
        rule <- mkRule (envKeys env) (cTokVersion corpus)
            (map T.pack match) (map T.pack repl) Nothing
        let n = countRuleMatches corpus rule
        if n == 0
            then die "no matches in the corpus; rule not written"
            else do
                path <- saveRule rule
                putStrLn ("wrote " <> path <> " (" <> show n <> " matches)")
    _ -> die "usage: overlay --mkrule <match-words...> => <replacement-words...>"
