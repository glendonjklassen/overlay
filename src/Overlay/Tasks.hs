{-# LANGUAGE OverloadedStrings #-}

module Overlay.Tasks where

import Control.Exception (SomeException, try)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (defaultTimeLocale, formatTime)
import System.Directory (doesFileExist, removeFile)

import Overlay.Bridge
import Overlay.Corpus
import Overlay.Patch
import Overlay.Refs
import Overlay.Rule
import Overlay.Thread
import Overlay.Types
import Overlay.Weave

-- | Persist the OT↔NT bridge approvals after an approve/reject.
saveBridgeTask :: BridgeStore -> IO AppEvent
saveBridgeTask store = do
    r <- try (saveApprovals bridgeFile store)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("bridge save failed: " <> showt e)
        Right () -> EvStatus "bridge updated"

saveTask :: Env -> EditTarget -> [Text] -> Maybe Text -> IO AppEvent
saveTask env et repl note = do
    r <- try $ do
        p <- mkPatch (envKeys env) (cTokVersion (envCorpus env))
            (etRef et) (etSpan et) (etWords et) repl note
        path <- savePatch p
        lps <- loadPatches (envKeys env) (envCorpus env)
        pure (lps, path)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("save failed: " <> showt e)
        Right (lps, path) -> EvPatchesLoaded lps ("saved " <> T.pack path)

deleteTask :: Env -> FilePath -> IO AppEvent
deleteTask env path = do
    r <- try $ do
        removeFile path
        loadPatches (envKeys env) (envCorpus env)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("delete failed: " <> showt e)
        Right lps -> EvPatchesLoaded lps ("deleted " <> T.pack path)

saveRuleTask :: Env -> EditTarget -> [Text] -> Maybe Text -> IO AppEvent
saveRuleTask env et repl note = do
    r <- try $ do
        rule <- mkRule (envKeys env) (cTokVersion (envCorpus env))
            (etWords et) repl note
        path <- saveRule rule
        lrs <- loadRules (envKeys env) (envCorpus env)
        pure (lrs, path)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("save failed: " <> showt e)
        Right (lrs, path) -> EvRulesLoaded lrs ("saved " <> T.pack path)

deleteRuleTask :: Env -> FilePath -> IO AppEvent
deleteRuleTask env path = do
    r <- try $ do
        removeFile path
        loadRules (envKeys env) (envCorpus env)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("delete failed: " <> showt e)
        Right lrs -> EvRulesLoaded lrs ("deleted " <> T.pack path)

excludeRuleTask :: Env -> FilePath -> (Text, Int, Int) -> IO AppEvent
excludeRuleTask env path ref = do
    r <- try (excludeVerse (envKeys env) path ref)
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("exclude failed: " <> showt e))
        Right (Left err) ->
            pure (EvStatus ("exclude failed: " <> T.pack err))
        Right (Right _) -> do
            lrs <- loadRules (envKeys env) (envCorpus env)
            pure (EvRulesLoaded lrs ("excluded " <> refText ref))

addThreadTask
    :: Env -> [LoadedThread] -> Text -> EditTarget -> Maybe Text -> IO AppEvent
addThreadTask env lts name et note = do
    r <- try $ addToThread lts name (cTokVersion (envCorpus env))
        (etRef et) (etSpan et) (etWords et) note
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("thread save failed: " <> showt e))
        Right (Left err) ->
            pure (EvStatus ("thread save failed: " <> T.pack err))
        Right (Right _) -> reloadThreads ("added to \x201C" <> name <> "\x201D")

reloadThreads :: Text -> IO AppEvent
reloadThreads msg = do
    (lts, errs) <- loadThreads
    pure (EvThreadsLoaded lts
        (msg <> if null errs then "" else " · " <> threadErrText errs))

threadErrText :: [String] -> Text
threadErrText errs = showt (length errs) <> " thread file(s) unreadable"

saveThreadNotesTask :: LoadedThread -> Text -> IO AppEvent
saveThreadNotesTask lt notes = do
    r <- try (writeThread (ltFile lt) (ltThread lt) { thNotes = notes })
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("save failed: " <> showt e))
        Right () -> reloadThreads "notes saved"

deleteThreadTask :: FilePath -> IO AppEvent
deleteThreadTask path = do
    r <- try (removeFile path)
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("delete failed: " <> showt e))
        Right () -> reloadThreads ("deleted " <> T.pack path)

deleteThreadEntryTask :: LoadedThread -> Int -> IO AppEvent
deleteThreadEntryTask lt entryIx = do
    let t = ltThread lt
        keep = [e | (i, e) <- zip [0 ..] (thEntries t), i /= entryIx]
    r <- try (writeThread (ltFile lt) t { thEntries = keep })
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("remove failed: " <> showt e))
        Right () -> reloadThreads "passage removed"

-- ── weave tasks ───────────────────────────────────────────────────────────────

reloadWeaves :: Text -> IO AppEvent
reloadWeaves msg = do
    (lws, errs) <- loadWeaves
    pure (EvWeavesLoaded lws
        (msg <> if null errs then "" else " · " <> weaveErrText errs))

weaveErrText :: [String] -> Text
weaveErrText errs = showt (length errs) <> " weave file(s) unreadable"

-- | Create a weave (optionally seeded with links), then reload.
newWeaveTask :: Text -> WeaveKind -> Text -> [Link] -> IO AppEvent
newWeaveTask name kind tokv links = do
    now <- getCurrentTime
    let stamp = T.pack (formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now)
        path = weaveFileFor name
        w = addLinks links (emptyWeave name kind tokv stamp)
    exists <- doesFileExist path
    if exists
        then pure (EvStatus ("a weave file already exists: " <> T.pack path))
        else do
            r <- try (writeWeave path w)
            case r of
                Left (e :: SomeException) ->
                    pure (EvStatus ("create failed: " <> showt e))
                Right () -> reloadWeaves ("created \x201C" <> name <> "\x201D")

editWeaveTask :: LoadedWeave -> (Weave -> Weave) -> Text -> IO AppEvent
editWeaveTask lw f msg = do
    r <- try (writeWeave (lwFile lw) (f (lwWeave lw)))
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("weave save failed: " <> showt e))
        Right () -> reloadWeaves msg

-- | Persist an already-updated weave without reloading. The model has already
-- been changed optimistically (in the event handler), so there is no reload
-- race when several edits land in quick succession — this just writes to disk.
saveWeaveTask :: FilePath -> Weave -> IO AppEvent
saveWeaveTask path w = do
    r <- try (writeWeave path w)
    pure $ case r of
        Left (e :: SomeException) -> EvStatus ("weave save failed: " <> showt e)
        Right () -> EvNoop

deleteWeaveTask :: FilePath -> IO AppEvent
deleteWeaveTask path = do
    r <- try (removeFile path)
    case r of
        Left (e :: SomeException) ->
            pure (EvStatus ("delete failed: " <> showt e))
        Right () -> reloadWeaves ("deleted " <> T.pack path)
