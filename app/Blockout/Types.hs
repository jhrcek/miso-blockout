{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

-- | Model, scenes, actions and the game's derived parameters.
module Blockout.Types where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Miso (MisoString)
import Miso.Lens.TH (makeLenses)

-----------------------------------------------------------------------------
-- Setup: the configurable game parameters of the original
-----------------------------------------------------------------------------

{- | A cell in the pit. x grows right, y grows down (screen), z grows away
from the viewer (deeper into the pit).
-}
type Cell = (Int, Int, Int)

-- | The three axes of the pit, in the order of the 'Cell' components.
data Axis = X | Y | Z
    deriving (Eq, Show)

-- | The two ways to make a 90 degree turn about an axis.
data Turn = CW | CCW
    deriving (Eq, Show)

-- | The sign of a turn, as used by the rotation animation.
turnSign :: Turn -> Double
turnSign = \case
    CW -> 1
    CCW -> -1

{- | The minimum and maximum corner of the bounding box of a non-empty set
of cells: every cell lies between the two, inclusive.
-}
bounds :: [Cell] -> (Cell, Cell)
bounds cs =
    ( (minimum xs, minimum ys, minimum zs)
    , (maximum xs, maximum ys, maximum zs)
    )
  where
    xs = [x | (x, _, _) <- cs]
    ys = [y | (_, y, _) <- cs]
    zs = [z | (_, _, z) <- cs]

data BlockSet = Flat | Basic | Extended
    deriving (Bounded, Enum, Eq, Show)

blockSetName :: BlockSet -> MisoString
blockSetName = \case
    Flat -> "FLAT"
    Basic -> "BASIC"
    Extended -> "EXTENDED"

-- | Speed of the rotation/transition animation. Never affects the score.
data RotationSpeed = Slow | Medium | Fast
    deriving (Bounded, Enum, Eq, Show)

speedName :: RotationSpeed -> MisoString
speedName = \case
    Slow -> "SLOW"
    Medium -> "MEDIUM"
    Fast -> "FAST"

-- | Duration of a 90 degree turn in seconds.
spinDuration :: RotationSpeed -> Double
spinDuration = \case
    Slow -> 0.35
    Medium -> 0.2
    Fast -> 0.1

data Setup = Setup
    { setupW :: Int
    -- ^ pit width (x), 3..7
    , setupL :: Int
    -- ^ pit length (y), 3..7
    , setupD :: Int
    -- ^ pit depth (z), 6..18
    , setupSet :: BlockSet
    , setupSpeed :: RotationSpeed
    }
    deriving (Eq, Show)

-- | The three predefined setups of the original game.
predefined :: NonEmpty (MisoString, Setup)
predefined =
    ("FLAT FUN", Setup 5 5 12 Flat Medium)
        :| [ ("3-D MANIA", Setup 3 3 10 Basic Medium)
           , ("OUT OF CONTROL", Setup 5 5 10 Extended Medium)
           ]

defaultSetup :: Setup
defaultSetup = snd (NE.head predefined)

-- | Position of a setup in 'predefined', if it is one of them.
predefIndex :: Setup -> Maybe Int
predefIndex s = case [i | (i, (_, p)) <- zip [0 ..] (NE.toList predefined), p == s] of
    (i : _) -> Just i
    [] -> Nothing

-- | The name of a predefined setup, or "CUSTOM" for anything else.
predefName :: Setup -> MisoString
predefName s = maybe "CUSTOM" (fst . (predefined NE.!!)) (predefIndex s)

pitVolume :: Setup -> Int
pitVolume s = setupW s * setupL s * setupD s

{- | Score multiplier that grows as the pit shrinks (manual: score increases
"as the size of the pit decreases"). 1 for the 5x5x12 Flat Fun pit.
-}
pitFactor :: Setup -> Int
pitFactor s = max 1 ((300 + v - 1) `div` v)
  where
    v = pitVolume s

-- | Manual: score increases "with the complexity of ... the block set".
setWeight :: BlockSet -> Int
setWeight = \case
    Flat -> 1
    Basic -> 2
    Extended -> 3

-----------------------------------------------------------------------------
-- Setup parameter ranges and small shared helpers
-----------------------------------------------------------------------------

{- | Inclusive ranges of the configurable pit dimensions. Shared by the
setup menu (which cycles within them) and "Blockout.Persist" (which
validates loaded values against them), so the two cannot drift apart.
-}
widthRange, lengthRange, depthRange :: (Int, Int)
widthRange = (3, 7)
lengthRange = (3, 7)
depthRange = (6, 18)

-- | Is a value within an inclusive range?
inRange :: (Int, Int) -> Int -> Bool
inRange (lo, hi) v = lo <= v && v <= hi

-- | Wrap a value into an inclusive range, cycling round at either end.
wrapRange :: (Int, Int) -> Int -> Int
wrapRange (lo, hi) v = lo + (v - lo) `mod` (hi - lo + 1)

-- | Step a bounded enum one position in the given direction, wrapping round.
cycleEnum :: (Bounded a, Enum a, Eq a) => Int -> a -> a
cycleEnum dir x
    | dir >= 0 = if x == maxBound then minBound else succ x
    | otherwise = if x == minBound then maxBound else pred x

-----------------------------------------------------------------------------
-- Scenes (menu system)
-----------------------------------------------------------------------------

data MenuItem = MenuStart | MenuSetup | MenuWrite | MenuPractice | MenuHelp
    deriving (Bounded, Enum, Eq, Show)

data FameItem = FameStart | FameSetup | FameMenu
    deriving (Bounded, Enum, Eq, Show)

-- | The rows of the setup menu: six values to edit, then three buttons.
data SetupRow
    = PredefRow
    | BlockSetRow
    | SpeedRow
    | WidthRow
    | LengthRow
    | DepthRow
    | StartRow
    | WriteRow
    | MenuRow
    deriving (Bounded, Enum, Eq, Show)

data SetupButton = StartB | WriteB | MenuB
    deriving (Eq, Show)

-- | The button a setup-menu row stands for, for the three rows that are one.
rowButton :: SetupRow -> Maybe SetupButton
rowButton = \case
    StartRow -> Just StartB
    WriteRow -> Just WriteB
    MenuRow -> Just MenuB
    _ -> Nothing

data Scene
    = -- | main menu with the highlighted item
      MenuScene MenuItem
    | -- | focused row and the draft setup being edited
      SetupScene SetupRow Setup
    | -- | pick the starting level, 0..9
      LevelScene Int
    | HelpScene
    | GameScene
    | -- | typing a hall of fame name after game over
      NameScene MisoString
    | -- | hall of fame table with the highlighted item
      FameScene FameItem
    deriving (Eq, Show)

{- | Cycle the value of a setup-menu row one step in the given direction
(+1 or -1). The button rows hold no value and are left alone.
-}
adjustRow :: SetupRow -> Int -> Setup -> Setup
adjustRow row dir s = case row of
    PredefRow ->
        let ps = NE.map snd predefined
         in case predefIndex s of
                Just i -> ps NE.!! ((i + dir) `mod` length ps)
                Nothing -> if dir >= 0 then NE.head ps else NE.last ps
    BlockSetRow -> s{setupSet = cycleEnum dir (setupSet s)}
    SpeedRow -> s{setupSpeed = cycleEnum dir (setupSpeed s)}
    WidthRow -> s{setupW = wrapRange widthRange (setupW s + dir)}
    LengthRow -> s{setupL = wrapRange lengthRange (setupL s + dir)}
    DepthRow -> s{setupD = wrapRange depthRange (setupD s + dir)}
    _ -> s

-----------------------------------------------------------------------------
-- Model
-----------------------------------------------------------------------------

data Status = Playing | Paused | Over
    deriving (Eq, Show)

{- | Transient state of the rotation animation. The logical piece cells
always hold the final orientation; rendering applies the remaining part
of the inverse rotation, which shrinks to nothing as progress reaches 1.
-}
data Spin = Spin
    { spinAxis :: Axis
    , spinTurn :: Turn
    , spinOff :: (Double, Double, Double)
    -- ^ old centroid minus new centroid (wall kicks shift the piece)
    , spinT :: Double
    -- ^ progress, 0 to 1
    }
    deriving (Eq, Show)

data Model = Model
    { _scene :: Scene
    , _setup :: Setup
    -- ^ the active setup
    , _startLevel :: Int
    -- ^ chosen starting level, 0..9
    , _practice :: Bool
    -- ^ practice mode: no gravity
    , _well :: [Cell]
    -- ^ cells locked into the pit
    , _piece :: [Cell]
    -- ^ absolute cells of the falling piece
    , _spin :: Maybe Spin
    -- ^ rotation animation in flight, if any
    , _pendingLock :: Bool
    -- ^ piece was dropped; locks after a short slide window
    , _score :: Int
    , _fame :: [(MisoString, Int)]
    -- ^ hall of fame of the active setup, best first
    , _cubes :: Int
    -- ^ cubes played
    , _cleared :: Int
    -- ^ layers cleared
    , _status :: Status
    , _ticks :: Int
    -- ^ gravity / lock-window tick accumulator
    }
    deriving Eq

initialModel :: Model
initialModel =
    Model
        { _scene = MenuScene MenuStart
        , _setup = defaultSetup
        , _startLevel = 0
        , _practice = False
        , _well = []
        , _piece = []
        , _spin = Nothing
        , _pendingLock = False
        , _score = 0
        , _fame = []
        , _cubes = 0
        , _cleared = 0
        , _status = Playing
        , _ticks = 0
        }

makeLenses ''Model

-----------------------------------------------------------------------------
-- Derived game parameters
-----------------------------------------------------------------------------

{- | 11 difficulty levels, 0..10. As in the original BlockOut, the level is
driven by the cumulative volume of /cleared/ cubes, not by cubes dropped. A
level-up triggers once the cubes cleared reach a threshold of @width * length
* 2@ per level. Since each cleared layer contributes @width * length@ cubes,
this reduces to one level bump for every 2 full layers cleared, regardless of
pit size.
-}
level :: Model -> Int
level m =
    min 10 (_startLevel m + (_cleared m * layerCubes) `div` cubesPerLevel (_setup m))
  where
    layerCubes = setupW (_setup m) * setupL (_setup m)

cubesPerLevel :: Setup -> Int
cubesPerLevel s = setupW s * setupL s * 2

{- | Gravity period in 100ms ticks for a given level. The original game's
drop speed decays roughly geometrically with the level (the time to fall
one layer multiplies by ~0.7 each level), so model it as such, floored at
2 ticks (0.2s) so the fastest levels stay playable.
-}
dropTicks :: Int -> Int
dropTicks lvl = max 2 (round (50 * 0.7 ^^ lvl :: Double))

-- | The slide window after a drop, in 100ms ticks (manual p.10 note).
lockTicks :: Int
lockTicks = 3

famePlaces :: Int
famePlaces = 10

maxNameLen :: Int
maxNameLen = 10

-- | Best score of the active setup's hall of fame.
fameBest :: Model -> Int
fameBest m = case _fame m of
    ((_, s) : _) -> s
    [] -> 0

-- | Center of mass of a set of cells, in lattice corner coordinates.
centroid :: [Cell] -> (Double, Double, Double)
centroid cs =
    ( avg [fromIntegral x | (x, _, _) <- cs]
    , avg [fromIntegral y | (_, y, _) <- cs]
    , avg [fromIntegral z | (_, _, z) <- cs]
    )
  where
    avg xs = sum xs / fromIntegral (length xs) + 0.5

-----------------------------------------------------------------------------
data Action
    = Boot
    | SetupLoaded (Maybe MisoString)
    | FameLoaded (Maybe MisoString)
    | Tick
    | -- | throttled requestAnimationFrame tick advancing the rotation animation
      SpinTick
    | -- | keyCode, auto-repeat flag and key string of a keydown event
      KeyDown (Int, Bool, MisoString)
    | NewPiece Int
    | -- | mouse: activate a main menu item
      Activate MenuItem
    | -- | mouse: pick a starting level and begin
      PickLevel Int
    | -- | mouse: focus a setup row and replace the draft
      SetupClick SetupRow Setup
    | -- | mouse: a setup-menu button, with the current draft
      SetupCommit SetupButton Setup
    | -- | mouse: activate a hall-of-fame menu item
      FameActivate FameItem
    deriving (Eq, Show)

{- | A key press as a mouse action, for on-screen buttons that stand in for
a key. Routing them through the keyboard handler keeps touch and keyboard
controls on a single code path, so the two cannot drift apart.
-}
pressKey :: Int -> Action
pressKey code = KeyDown (code, False, "")
