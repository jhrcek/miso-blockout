{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Update logic: the menu system and the game itself.
module Blockout.Update
    ( updateModel
    , keyDecoder
    , gravitySub
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, when)
import Data.Char (isPrint)
import Data.Foldable (for_)
import Data.List (insertBy)
import Data.Maybe (isNothing)
import Data.Ord (Down (..), comparing)
import Miso hiding (status, (!!))
import qualified Miso.Event.Decoder as D
import Miso.JSON (withObject, (.!=), (.:), (.:?))
import Miso.Lens
import Miso.Random (replicateRM)

import Blockout.Persist
import Blockout.Pieces
import Blockout.Types

-----------------------------------------------------------------------------
-- Subscriptions
-----------------------------------------------------------------------------

{- | Decodes a keydown event into its keyCode, the auto-repeat flag and the
key string (used for name entry). Acting on raw keydown events (rather
than diffing a set of currently pressed keys) means a physical key press
can never be swallowed by stale key-tracking state, e.g. when a keyup got
lost while the window was unfocused.
-}
keyDecoder :: D.Decoder (Int, Bool, MisoString)
keyDecoder = D.at [] $ withObject "event" $ \o ->
    (,,)
        <$> o .: "keyCode"
        <*> (o .:? "repeat" .!= False)
        <*> (o .:? "key" .!= "")

-- | 100ms heartbeat driving gravity and the post-drop slide window.
gravitySub :: Sub Action
gravitySub sink = forever (threadDelay 100000 >> sink Tick)

{- | Drives the rotation animation via @requestAnimationFrame@, firing once
per frame with a @DOMHighResTimeStamp@ (ms). The update folds in the real
elapsed time between frames, so a turn lasts exactly its configured duration
regardless of the display's refresh rate (a fixed per-tick step would run
fast on 120Hz screens and stutter under load). Started when a rotation begins
and stopped once the animation has played out.
-}
spinSub :: Sub Action
spinSub = rAFSub SpinTick

spinKey :: MisoString
spinKey = "spin"

-----------------------------------------------------------------------------
-- Update
-----------------------------------------------------------------------------
updateModel :: Action -> Effect parent props Model Action
updateModel = \case
    Boot ->
        io (SetupLoaded <$> getLocalStorage setupKey)
    SetupLoaded stored -> do
        for_ (decodeSetup =<< stored) (setup .=)
        loadFame
    FameLoaded stored ->
        fame .= maybe [] decodeFame stored
    Tick -> gameTick
    SpinTick ts -> do
        m <- use this
        case _spin m of
            Nothing -> stopSub spinKey
            Just sp -> case spinLast sp of
                -- first frame: anchor the clock, advance on the next one
                Nothing -> spin .= Just sp{spinLast = Just ts}
                Just prev -> do
                    let dt = (ts - prev) / 1000 -- ms to seconds
                        step = dt / spinDuration (setupSpeed (_setup m))
                    if spinT sp + step >= 1
                        then do
                            spin .= Nothing
                            stopSub spinKey
                        else spin .= Just sp{spinT = spinT sp + step, spinLast = Just ts}
    KeyDown (code, isRepeat, key) ->
        unless isRepeat (handleKey code key)
    NewPiece i -> do
        m <- use this
        let ps = spawnable (_setup m)
            proto = ps !! min i (length ps - 1)
            cells = spawnCells (_setup m) proto
        spin .= Nothing
        pendingLock .= False
        if fits (_setup m) (_well m) cells
            then do
                piece .= cells
                ticks .= 0
            else do
                piece .= []
                status .= Over
    Activate item -> runMenu item
    PickLevel n -> do
        startLevel .= n
        startGame False
    SetupClick i draft -> scene .= SetupScene i draft
    SetupCommit b draft -> setupButton b draft
    FameActivate item -> runFame item

loadFame :: Effect parent props Model Action
loadFame = do
    s <- use setup
    io (FameLoaded <$> getLocalStorage (fameKey s))

-----------------------------------------------------------------------------
-- Scene transitions
-----------------------------------------------------------------------------

resetGame :: Effect parent props Model Action
resetGame = do
    well .= []
    piece .= []
    spin .= Nothing
    pendingLock .= False
    score .= 0
    cubes .= 0
    cleared .= 0
    ticks .= 0
    status .= Playing

startGame :: Bool -> Effect parent props Model Action
startGame practiceMode = do
    resetGame
    practice .= practiceMode
    scene .= GameScene
    spawnPiece

gotoMenu :: Effect parent props Model Action
gotoMenu = scene .= MenuScene MenuStart

abortToMenu :: Effect parent props Model Action
abortToMenu = resetGame >> gotoMenu

runMenu :: MenuItem -> Effect parent props Model Action
runMenu = \case
    MenuStart -> do
        n <- use startLevel
        scene .= LevelScene n
    MenuSetup -> do
        s <- use setup
        scene .= SetupScene 0 s
    MenuWrite -> do
        s <- use setup
        io_ (setLocalStorage setupKey (encodeSetup s))
    MenuPractice -> startGame True
    MenuHelp -> scene .= HelpScene

runFame :: FameItem -> Effect parent props Model Action
runFame = \case
    FameStart -> startGame False
    FameSetup -> do
        resetGame
        s <- use setup
        scene .= SetupScene 0 s
    FameMenu -> abortToMenu

setupButton :: SetupButton -> Setup -> Effect parent props Model Action
setupButton b draft = case b of
    StartB -> do
        applyDraft
        startGame False
    WriteB -> do
        applyDraft
        io_ (setLocalStorage setupKey (encodeSetup draft))
    MenuB -> do
        applyDraft
        gotoMenu
  where
    applyDraft = do
        old <- use setup
        setup .= draft
        when (fameKey old /= fameKey draft) loadFame

{- | The game is over and the player pressed Enter:
if we're not in practice mode, enter the hall of fame
if the score makes the top ten, otherwise just show it.
-}
finishGame :: Effect parent props Model Action
finishGame = do
    m <- use this
    let lowest
            | length (_fame m) < famePlaces = 0
            | otherwise = minimum (map snd (_fame m))
        qualifies =
            not (_practice m)
                && _score m > 0
                && _score m > lowest
    scene .= if qualifies then NameScene "" else FameScene FameStart

-----------------------------------------------------------------------------
-- Keyboard, dispatched by scene
-----------------------------------------------------------------------------

handleKey :: Int -> MisoString -> Effect parent props Model Action
handleKey code key = do
    m <- use this
    case _scene m of
        -- These screens consume the raw key themselves, so they take
        -- precedence over the global F shortcut below: name entry needs the
        -- letter keys, and help returns to the menu on any key.
        NameScene name -> nameKey name code key
        HelpScene -> gotoMenu
        -- F goes truly fullscreen from anywhere else (exit with the browser's
        -- native Esc). Requesting fullscreen needs a user gesture, which this
        -- keydown provides.
        _ | code == 70 -> io_ requestFullscreen
        MenuScene item -> menuKey item code
        SetupScene i draft -> setupSceneKey i draft code
        LevelScene n -> levelKey n code
        GameScene -> gameKey m code
        FameScene item -> fameSceneKey item code

menuKey :: MenuItem -> Int -> Effect parent props Model Action
menuKey item = \case
    38 -> scene .= MenuScene (cycleEnum (-1) item)
    40 -> scene .= MenuScene (cycleEnum 1 item)
    13 -> runMenu item
    -- first-letter shortcuts, as in the original menus
    83 -> runMenu MenuStart -- S
    67 -> runMenu MenuSetup -- C
    87 -> runMenu MenuWrite -- W
    80 -> runMenu MenuPractice -- P
    72 -> runMenu MenuHelp -- H
    _ -> pure ()

levelKey :: Int -> Int -> Effect parent props Model Action
levelKey n code
    | code >= 48 && code <= 57 = pick (code - 48)
    | code >= 96 && code <= 105 = pick (code - 96)
    | otherwise = case code of
        (37; 38) -> move (-1)
        (39; 40) -> move 1
        13 -> pick n
        27 -> scene .= MenuScene MenuStart
        _ -> pure ()
  where
    move d = scene .= LevelScene ((n + d) `mod` 10)
    pick lvl = do
        startLevel .= lvl
        startGame False

-- | Rows 0-5 are setup values, 6-8 the Start/Write/Menu buttons.
setupSceneKey :: Int -> Setup -> Int -> Effect parent props Model Action
setupSceneKey i draft = \case
    38 -> scene .= SetupScene ((i - 1) `mod` 9) draft
    40 -> scene .= SetupScene ((i + 1) `mod` 9) draft
    37 -> change (-1)
    39 -> change 1
    13
        | i <= 5 -> change 1
        | otherwise ->
            setupButton (case i of 6 -> StartB; 7 -> WriteB; _ -> MenuB) draft
    27 -> scene .= MenuScene MenuSetup -- cancel, discarding the draft
    _ -> pure ()
  where
    change d = when (i <= 5) (scene .= SetupScene i (adjustRow i d draft))

fameSceneKey :: FameItem -> Int -> Effect parent props Model Action
fameSceneKey item = \case
    38 -> scene .= FameScene (cycleEnum (-1) item)
    40 -> scene .= FameScene (cycleEnum 1 item)
    13 -> runFame item
    83 -> runFame FameStart -- S
    67 -> runFame FameSetup -- C
    77 -> runFame FameMenu -- M
    27 -> abortToMenu
    _ -> pure ()

nameKey :: MisoString -> Int -> MisoString -> Effect parent props Model Action
nameKey name code key = case code of
    13 -> do
        m <- use this
        let entries =
                take famePlaces $
                    insertBy (comparing (Down . snd)) (name, _score m) (_fame m)
        fame .= entries
        io_ (setLocalStorage (fameKey (_setup m)) (encodeFame entries))
        scene .= FameScene FameStart
    27 -> scene .= FameScene FameStart
    8 -> scene .= NameScene (ms (dropLast (fromMisoString name)))
    _ -> case fromMisoString key of
        [c]
            | isPrint c && length (fromMisoString name :: String) < maxNameLen ->
                scene .= NameScene (name <> key)
        _ -> pure ()
  where
    dropLast s = take (length s - 1) (s :: String)

gameKey :: Model -> Int -> Effect parent props Model Action
gameKey m code = case _status m of
    Over -> case code of
        -- practice mode has no hall of fame; only Esc, back to the menu
        13 | not (_practice m) -> finishGame
        27 -> abortToMenu
        _ -> pure ()
    Paused -> case code of
        80 -> status .= Playing
        27 -> abortToMenu
        _ -> pure ()
    Playing -> case code of
        -- move: arrows, numpad 4/6/8/2 (NumLock on) and the digit row
        (37; 100; 52) -> tryMove (-1) 0
        (39; 102; 54) -> tryMove 1 0
        (38; 104; 56) -> tryMove 0 (-1)
        (40; 98; 50) -> tryMove 0 1
        -- diagonals: numpad 7/9/1/3, Home/PgUp/End/PgDn and the digit row
        (103; 36; 55) -> tryMove (-1) (-1)
        (105; 33; 57) -> tryMove 1 (-1)
        (97; 35; 49) -> tryMove (-1) 1
        (99; 34; 51) -> tryMove 1 1
        32 -> hardDrop
        -- Q/W/E counter-clockwise, A/S/D clockwise about X/Y/Z (manual p.9);
        -- the Q/A and W/S pairs are flipped here so the on-screen turn
        -- matches the original game
        81 -> tryRotate 0 1 rotXcw
        65 -> tryRotate 0 (-1) rotXccw
        87 -> tryRotate 1 1 rotYcw
        83 -> tryRotate 1 (-1) rotYccw
        69 -> tryRotate 2 (-1) rotZccw
        68 -> tryRotate 2 1 rotZcw
        80 -> status .= Paused
        27 -> abortToMenu
        _ -> pure ()

-----------------------------------------------------------------------------
-- Gameplay
-----------------------------------------------------------------------------

fits :: Setup -> [Cell] -> [Cell] -> Bool
fits s w = all ok
  where
    ok c@(x, y, z) =
        x >= 0
            && x < setupW s
            && y >= 0
            && y < setupL s
            && z >= 0
            && z < setupD s
            && c `notElem` w

spawnPiece :: Effect parent props Model Action
spawnPiece = do
    m <- use this
    let n = length (spawnable (_setup m))
    io $ do
        ds <- replicateRM 1
        let d = case ds of (x : _) -> x; [] -> 0.5
        pure (NewPiece (floor (d * fromIntegral n)))

gameTick :: Effect parent props Model Action
gameTick = do
    m <- use this
    when (inPlay m) $
        if _pendingLock m
            then do
                -- the post-drop slide window (manual p.10 note)
                let t = _ticks m + 1
                if t >= lockTicks
                    then do
                        ticks .= 0
                        slamLock
                    else ticks .= t
            else unless (_practice m) $ do
                -- practice mode: blocks do not descend automatically
                let t = _ticks m + 1
                if t >= dropTicks (level m)
                    then do
                        ticks .= 0
                        stepDown
                    else ticks .= t
  where
    inPlay m =
        _scene m == GameScene
            && _status m == Playing
            && not (null (_piece m))

tryMove :: Int -> Int -> Effect parent props Model Action
tryMove dx dy = do
    m <- use this
    let moved = [(x + dx, y + dy, z) | (x, y, z) <- _piece m]
    when (fits (_setup m) (_well m) moved) (piece .= moved)

down :: Int -> [Cell] -> [Cell]
down k cs = [(x, y, z + k) | (x, y, z) <- cs]

-- | How far the piece can still fall.
maxDescent :: Model -> Int
maxDescent m = descend 0
  where
    descend k
        | fits (_setup m) (_well m) (down (k + 1) (_piece m)) = descend (k + 1)
        | otherwise = k

stepDown :: Effect parent props Model Action
stepDown = do
    m <- use this
    let moved = down 1 (_piece m)
    if fits (_setup m) (_well m) moved
        then piece .= moved
        else lockPiece

{- | Drop the piece. It lands but locks only after a short window during
which it can still be moved. The drop height earns points (manual p.12).
A second Space during the window locks immediately.
-}
hardDrop :: Effect parent props Model Action
hardDrop = do
    m <- use this
    unless (null (_piece m)) $
        if _pendingLock m
            then do
                ticks .= 0
                slamLock
            else do
                let dist = maxDescent m
                piece .= down dist (_piece m)
                score += dist * (level m + 1)
                pendingLock .= True
                ticks .= 0

{- | End of the slide window: if the piece was moved over a hole it falls
the rest of the way, then locks.
-}
slamLock :: Effect parent props Model Action
slamLock = do
    m <- use this
    piece .= down (maxDescent m) (_piece m)
    lockPiece

lockPiece :: Effect parent props Model Action
lockPiece = do
    m <- use this
    let s = _setup m
        w0 = _piece m ++ _well m
        full =
            [ z
            | z <- [0 .. setupD s - 1]
            , length [() | (_, _, cz) <- w0, cz == z] == setupW s * setupL s
            ]
        w1 =
            [ (x, y, z + length (filter (> z) full))
            | (x, y, z) <- w0
            , z `notElem` full
            ]
        lvl = level m
        pf = pitFactor s
        n = length full
        pieceScore = length (_piece m) * (lvl + 1) * setWeight (setupSet s) * pf
        layerScore = 100 * (lvl + 1) * n * n * pf
        -- emptying the whole pit earns a big bonus (manual p.12)
        clearBonus
            | n > 0 && null w1 = 1000 * (lvl + 1) * pf
            | otherwise = 0
    well .= w1
    piece .= []
    spin .= Nothing
    pendingLock .= False
    cubes += length (_piece m)
    cleared += n
    score += pieceScore + layerScore + clearBonus
    spawnPiece

-----------------------------------------------------------------------------
-- Rotation. Pieces rotate about the center of their bounding box, with a
-- few "kick" offsets tried so rotation works next to walls.
-----------------------------------------------------------------------------
type Dims = (Int, Int, Int)

rotXcw, rotXccw, rotYcw, rotYccw, rotZcw, rotZccw :: Dims -> Cell -> Cell
rotXcw (_, _, sz) (x, y, z) = (x, sz - 1 - z, y)
rotXccw (_, sy, _) (x, y, z) = (x, z, sy - 1 - y)
rotYcw (_, _, sz) (x, y, z) = (sz - 1 - z, y, x)
rotYccw (sx, _, _) (x, y, z) = (z, y, sx - 1 - x)
rotZcw (_, sy, _) (x, y, z) = (sy - 1 - y, x, z)
rotZccw (sx, _, _) (x, y, z) = (y, sx - 1 - x, z)

{- | Round @n@/2 to the nearest integer, breaking ties away from zero.
Recentering a rotated piece with this (rather than 'div', which floors)
keeps rotation a true cyclic action: the four offsets accumulated over a
full turn sum to zero, so repeating any rotation key returns the piece to
its starting cells instead of drifting sideways.
-}
roundHalf :: Int -> Int
roundHalf n = signum n * ((abs n + 1) `div` 2)

{- | Attempt a rotation. @axis@ (0 = X, 1 = Y, 2 = Z) and @dir@ describe
the same turn as the discrete @rot@ function and are used to animate it.

The piece turns about the centre of its bounding box. If it does not fit
in place it is nudged back inside the pit: sideways off a wall, or
downward when a piece that grew taller would poke out through the mouth.
Upward kicks are deliberately excluded, as they would let repeated presses
of one rotation key climb the piece back up against gravity. Away from the
walls the in-place rotation always fits, so repeating any rotation key
cycles the piece through its orientations and back to its starting cells.
-}
tryRotate :: Int -> Double -> (Dims -> Cell -> Cell) -> Effect parent props Model Action
tryRotate axis dir rot = do
    m <- use this
    unless (null (_piece m)) $ do
        let cs = _piece m
            mnx = minimum [x | (x, _, _) <- cs]
            mny = minimum [y | (_, y, _) <- cs]
            mnz = minimum [z | (_, _, z) <- cs]
            sx = maximum [x | (x, _, _) <- cs] - mnx + 1
            sy = maximum [y | (_, y, _) <- cs] - mny + 1
            sz = maximum [z | (_, _, z) <- cs] - mnz + 1
            rel' = [rot (sx, sy, sz) (x - mnx, y - mny, z - mnz) | (x, y, z) <- cs]
            sx' = maximum [x | (x, _, _) <- rel'] + 1
            sy' = maximum [y | (_, y, _) <- rel'] + 1
            sz' = maximum [z | (_, _, z) <- rel'] + 1
            ox = mnx + roundHalf (sx - sx')
            oy = mny + roundHalf (sy - sy')
            oz = mnz + roundHalf (sz - sz')
            -- Try the rotation in place first, then nudge it back inside
            -- the pit: sideways off a wall, or downward (only as far as is
            -- needed to clear the mouth, z >= 0). Never upward.
            kicks =
                (0, 0, 0)
                    : [ (kx, ky, 0)
                      | (kx, ky) <-
                            [ (-1, 0)
                            , (1, 0)
                            , (0, -1)
                            , (0, 1)
                            , (-2, 0)
                            , (2, 0)
                            , (0, -2)
                            , (0, 2)
                            ]
                      ]
                    ++ [(0, 0, kz) | kz <- [1 .. max 0 (negate oz)]]
            attempts =
                [ [(x + ox + kx, y + oy + ky, z + oz + kz) | (x, y, z) <- rel']
                | (kx, ky, kz) <- kicks
                ]
        case filter (fits (_setup m) (_well m)) attempts of
            (good : _) -> do
                let (px, py, pz) = centroid cs
                    (gx, gy, gz) = centroid good
                piece .= good
                spin .= Just (Spin axis dir (px - gx, py - gy, pz - gz) 0 Nothing)
                when (isNothing (_spin m)) (startSub spinKey spinSub)
            [] -> pure ()
