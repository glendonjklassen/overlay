{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Overlay.Types where

import Control.Lens
import qualified Data.Map.Strict as M
import Data.Text (Text)

import Overlay.Bridge
import Overlay.Concept
import Overlay.Config
import Overlay.Corpus
import Overlay.Patch
import Overlay.ReaderView
import Overlay.Rule
import Overlay.Strongs
import Overlay.Thread
import Overlay.Weave

-- ── environment and model ───────────────────────────────────────────────────

-- Static environment shared by the whole UI (not part of the model: it never
-- changes, and keeping it out avoids Eq checks over megabytes of text).
data Env = Env
    { envCorpus   :: Corpus
    , envStrongs  :: StrongsDict
    , envOccIx    :: OccurrenceIx
    , envConcept  :: ConceptIx       -- ^ per-Strong's counts / distribution / rarity
    , envBridge   :: Bridge          -- ^ OT↔NT etymology links (static; approvals live in the model)
    , envBridgeCands :: M.Map Text [RenderCand]  -- ^ rendering candidates indexed by lemma
    , envBridgeExtra :: M.Map Text [SourceLink]  -- ^ hydrated external source links by lemma
    , envSuggestions :: [Suggestion]  -- ^ cached shared-lemma-run parallels to review
    , envKeys     :: Keys
    , envNotes    :: M.Map (Text, Int, Int) [Text]
    , envSettings :: Settings
    }

data EditTarget = EditTarget
    { etRef     :: (Text, Int, Int)
    , etSpan    :: (Int, Int)
    , etWords   :: [Text]
    , etMatches :: Int  -- ^ corpus-wide occurrences of the span's words
    , etRuleHit :: Maybe (FilePath, Text, Bool)
      -- ^ (file, \"match → repl\", is ours) when a rule rewrites this span
    } deriving (Eq, Show)

data PanelMode
    = PNone
    | POptions                             -- ^ central display/reading options
    | PStrongs Text Text (Text, Int, Int)  -- ^ clicked word, Strong's ref, verse
    | PEdit EditTarget
    | PPatches
    | PThreads
    | PThreadView FilePath
    | PWeaves
    | PWeaveView FilePath       -- ^ inspect / edit one weave
    | PSuggestions              -- ^ review auto-detected parallel candidates
    deriving (Eq, Show)

-- | A reading pane: where it points, plus the verses currently selected there
-- (anchored, so Shift-click extends from the anchor). One pane is ordinary
-- reading; several are parallel passages.
data PaneState = PaneState
    { _psBook    :: !Text
    , _psChapter :: !Int
    , _psAnchor  :: !(Maybe Int)  -- ^ last verse clicked, for Shift-extend
    , _psSel     :: ![Int]        -- ^ selected verse numbers
    } deriving (Eq, Show)

data AppModel = AppModel
    { _amPanel       :: PanelMode
    , _amNotesOn     :: Bool
    , _amHeatmapOn   :: Bool  -- ^ shade verses by their number of weave witnesses
    , _amLinesOn     :: Bool  -- ^ draw the weave connector lines across panes
    , _amPatches     :: [LoadedPatch]
    , _amRules       :: [LoadedRule]
    , _amThreads     :: [LoadedThread]
    , _amReplace     :: Text
    , _amNote        :: Text
    , _amEverywhere  :: Bool  -- ^ editor scope: save as rule, not patch
    , _amThreadPick  :: Text  -- ^ existing thread chosen in the editor
    , _amThreadNew   :: Text  -- ^ new thread name typed in the editor
    , _amThreadNotes :: Text  -- ^ notes draft for the open thread
    , _amStatus      :: Text
    -- weaves: a graph of verse links, shown across reading panes
    , _amPanes       :: [PaneState]
    , _amPrevPanes   :: Maybe [PaneState]
      -- ^ reading layout stashed when a weave reshaped the panes, so closing the
      -- weave can put it back
    , _amWeaves      :: [LoadedWeave]
    , _amWeaveNew    :: Text       -- ^ new weave name
    , _amWeaveKind   :: WeaveKind  -- ^ draft kind for the next new / linked weave
    , _amWeaveViewKind :: WeaveKind  -- ^ kind of the weave currently inspected
    , _amWeaveNotes  :: Text       -- ^ notes draft for the inspected weave
    , _amCombinePick :: Text       -- ^ weave to combine into the inspected one
    , _amCompare     :: Maybe ((Text, Int, Int), Double, Double)
      -- ^ hovered linked verse + window x,y, for the floating compare card
    , _amBodySize    :: Double   -- ^ live scripture text size (Ctrl +/-/0 zoom)
    , _amMaxCols     :: Int      -- ^ live cap on reading columns, 1…maxColsCap
    , _amActivePane  :: Int      -- ^ last pane the user acted in; cross-reference
                                 -- jumps and the canon map target it
    , _amLineSpacing :: Double   -- ^ live line spacing, persisted to config.json
    , _amConcepts    :: [Text]   -- ^ active Strong's numbers shown on the concept
                                  -- dispersion strip (1…4); empty hides it
    , _amBridge      :: BridgeStore  -- ^ user's OT↔NT rendering-link approvals
    , _amBridgeExtraOn :: Bool   -- ^ include opt-in external bridge sources
                                  -- (LXX, semantic domains); off by default
    , _amPinnedConcepts :: [Text]  -- ^ concepts pinned onto the dispersion strip
                                  -- for comparison (persist as you browse)
    } deriving (Eq, Show)

data AppEvent
    = EvInit
    | EvWordClicked RTok
    | EvWordAlt RTok
    | EvSpanSelected (Text, Int, Int) (Int, Int)
    | EvVerseClicked Int (Text, Int, Int) Bool
    | EvGoRef Text Int
    | EvClosePanel
    | EvToggleOptions
    | EvTogglePatches
    | EvSavePatch
    | EvDeletePatch FilePath
    | EvPatchesLoaded [LoadedPatch] Text
    | EvDeleteRule FilePath
    | EvExcludeRule FilePath (Text, Int, Int)
    | EvRulesLoaded [LoadedRule] Text
    | EvToggleThreads
    | EvShowThreads
    | EvOpenThread FilePath
    | EvAddToThread
    | EvSaveThreadNotes FilePath
    | EvDeleteThread FilePath
    | EvDeleteThreadEntry FilePath Int
    | EvThreadsLoaded [LoadedThread] Text
    -- panes
    | EvAddPane Int
    | EvClosePane Int
    | EvPaneBook Int Text
    | EvPaneChapter Int Int
    | EvPanePrev Int
    | EvPaneNext Int
    | EvPaneTrack Int (Text, Int)  -- ^ point pane i at one of a weave's passages
    | EvSetMaxCols Int             -- ^ change the live reading-column limit
    | EvCanonGoto Double           -- ^ jump the active pane to a canon fraction 0…1
    | EvLineSpacing Double         -- ^ nudge line spacing by a delta (0 = reset)
    -- OT↔NT bridge approvals (canonical (Hebrew, Greek) pair)
    | EvBridgeApprove Text Text    -- ^ approve a rendering bridge link (H, G)
    | EvBridgeReject Text Text     -- ^ reject a rendering bridge link (H, G)
    -- concept dispersion strip comparison
    | EvPinConcept Text            -- ^ pin a Strong's number onto the strip
    | EvClearPins                  -- ^ clear the pinned comparison
    -- suggested parallels (auto-detected within-language shared-lemma runs)
    | EvToggleSuggestions
    | EvOpenSuggestion (Text, Int, Int) (Text, Int, Int)  -- ^ show the pair in panes
    | EvAcceptSuggestion Suggestion  -- ^ accept → a new unapproved weave
    -- weaves
    | EvToggleWeaves
    | EvShowWeaves
    | EvOpenWeave FilePath
    | EvNewWeave
    | EvLink
    | EvSetWeaveKind WeaveKind
    | EvSaveWeaveNotes
    | EvRemoveLink Link
    | EvApproveLink Link Bool
    | EvApproveWeave Bool
    | EvCombineWeave Text
    | EvDeleteWeave FilePath
    | EvWeavesLoaded [LoadedWeave] Text
    | EvStatus Text
    | EvSaveSession
    | EvVerseInspect (Text, Int, Int) Double Double
    | EvCloseCompare
    | EvApproveLinkIn FilePath Link Bool
    | EvRejectLinkIn FilePath Link
    | EvZoom Double  -- ^ change scripture text size by delta px (0 = reset)
    | EvNoop
    deriving (Eq, Show)

makeLenses ''PaneState
makeLenses ''AppModel

-- | A pane's passage as a single (book, chapter) value, so a dropdown can bind
-- to it directly. Writing one clears any in-progress verse selection, like the
-- other navigation paths.
psTrack :: Lens' PaneState (Text, Int)
psTrack = lens (\p -> (_psBook p, _psChapter p))
    (\p (b, c) -> p { _psBook = b, _psChapter = c, _psAnchor = Nothing, _psSel = [] })

-- | UI scale factor for the chrome: text rides the same zoom as the scripture
-- body (1.0 at the default size), so buttons/labels/panels grow with Ctrl+/-.
uiScaleOf :: AppModel -> Double
uiScaleOf m = _amBodySize m / sBodySize defaultSettings
