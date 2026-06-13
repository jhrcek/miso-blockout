{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Model, scenes, actions and the game's derived parameters.
module Blockout.Types where

import Data.List.NonEmpty (NonEmpty (..))
import qualified Data.List.NonEmpty as NE
import Miso (MisoString)
import Miso.Lens

-----------------------------------------------------------------------------
-- Setup: the configurable game parameters of the original
-----------------------------------------------------------------------------

{- | A cell in the pit. x grows right, y grows down (screen), z grows away
from the viewer (deeper into the pit).
-}
type Cell = (Int, Int, Int)

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
-- Scenes (menu system)
-----------------------------------------------------------------------------

data MenuItem = MenuStart | MenuSetup | MenuWrite | MenuPractice | MenuHelp
    deriving (Bounded, Enum, Eq, Show)

data FameItem = FameStart | FameSetup | FameMenu
    deriving (Bounded, Enum, Eq, Show)

data SetupButton = StartB | WriteB | MenuB
    deriving (Eq, Show)

data Scene
    = -- | main menu with the highlighted item
      MenuScene MenuItem
    | -- | focused row index and the draft setup being edited
      SetupScene Int Setup
    | -- | pick the starting level, 0..9
      LevelScene Int
    | HelpScene
    | GameScene
    | -- | typing a hall of fame name after game over
      NameScene MisoString
    | -- | hall of fame table with the highlighted item
      FameScene FameItem
    deriving (Eq, Show)

{- | Cycle the value of a setup-menu row. Rows: 0 predefined setup,
1 block set, 2 rotation speed, 3 width, 4 length, 5 depth.
-}
adjustRow :: Int -> Int -> Setup -> Setup
adjustRow row dir s = case row of
    0 ->
        let ps = NE.map snd predefined
         in case [i | (i, p) <- zip [0 ..] (NE.toList ps), p == s] of
                (i : _) -> ps NE.!! ((i + dir) `mod` length ps)
                [] -> if dir >= 0 then NE.head ps else NE.last ps
    1 -> s{setupSet = cyc (setupSet s)}
    2 -> s{setupSpeed = cyc (setupSpeed s)}
    3 -> s{setupW = wrap 3 7 (setupW s + dir)}
    4 -> s{setupL = wrap 3 7 (setupL s + dir)}
    5 -> s{setupD = wrap 6 18 (setupD s + dir)}
    _ -> s
  where
    cyc :: (Bounded a, Enum a, Eq a) => a -> a
    cyc x
        | dir >= 0 = if x == maxBound then minBound else succ x
        | otherwise = if x == minBound then maxBound else pred x
    wrap lo hi v = lo + (v - lo) `mod` (hi - lo + 1)

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
    { spinAxis :: Int
    -- ^ 0 = X, 1 = Y, 2 = Z
    , spinDir :: Double
    -- ^ +1 or -1, the sign of the 90 degree turn
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

scene :: Lens Model Scene
scene = lens _scene (\r f -> r{_scene = f})

setup :: Lens Model Setup
setup = lens _setup (\r f -> r{_setup = f})

startLevel :: Lens Model Int
startLevel = lens _startLevel (\r f -> r{_startLevel = f})

practice :: Lens Model Bool
practice = lens _practice (\r f -> r{_practice = f})

well :: Lens Model [Cell]
well = lens _well (\r f -> r{_well = f})

piece :: Lens Model [Cell]
piece = lens _piece (\r f -> r{_piece = f})

spin :: Lens Model (Maybe Spin)
spin = lens _spin (\r f -> r{_spin = f})

pendingLock :: Lens Model Bool
pendingLock = lens _pendingLock (\r f -> r{_pendingLock = f})

score :: Lens Model Int
score = lens _score (\r f -> r{_score = f})

fame :: Lens Model [(MisoString, Int)]
fame = lens _fame (\r f -> r{_fame = f})

cubes :: Lens Model Int
cubes = lens _cubes (\r f -> r{_cubes = f})

cleared :: Lens Model Int
cleared = lens _cleared (\r f -> r{_cleared = f})

status :: Lens Model Status
status = lens _status (\r f -> r{_status = f})

ticks :: Lens Model Int
ticks = lens _ticks (\r f -> r{_ticks = f})

-----------------------------------------------------------------------------
-- Derived game parameters
-----------------------------------------------------------------------------

{- | 11 difficulty levels, 0..10. Larger pits need more cubes per level
(manual: "In larger pits, you must drop more cubes before the difficulty
level changes").
-}
level :: Model -> Int
level m = min 10 (_startLevel m + _cubes m `div` cubesPerLevel (_setup m))

cubesPerLevel :: Setup -> Int
cubesPerLevel s = max 12 (pitVolume s `div` 12)

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
    | SpinTick
    | KeyDown (Int, Bool, MisoString)
    | NewPiece Int
    | -- | mouse: activate a main menu item
      Activate MenuItem
    | -- | mouse: pick a starting level and begin
      PickLevel Int
    | -- | mouse: focus a setup row and replace the draft
      SetupClick Int Setup
    | -- | mouse: a setup-menu button, with the current draft
      SetupCommit SetupButton Setup
    | -- | mouse: activate a hall-of-fame menu item
      FameActivate FameItem
    deriving (Eq, Show)
