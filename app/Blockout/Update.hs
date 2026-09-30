{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Update logic: the menu system and the game itself.
module Blockout.Update
    ( updateModel
    , keyDecoder
    , gravitySub
    , spinSub
    ) where

import Control.Concurrent (threadDelay)
import Control.Monad (forever, unless, when)
import Data.Char (isPrint)
import Data.Foldable (for_)
import Data.List (insertBy)
import Data.Ord (Down (..), comparing)
import qualified Data.Set as Set
import Miso hiding (status, (!!))
import qualified Miso.Event.Decoder as D
import Miso.JSON (withObject, (.!=), (.:), (.:?))
import Miso.Lens
import Miso.Random (replicateRM)

import Blockout.Key
import Blockout.Mechanics
import Blockout.Persist
import Blockout.Pieces
import Blockout.Solver
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
gravitySub :: Sub Model Action
gravitySub sink _ = forever (threadDelay 100000 >> sink Tick)

{- | Drives the rotation animation off the browser's @requestAnimationFrame@
loop, throttled to 'spinIntervalMs'. Each tick advances the spin by a fixed
step, so a turn looks the same regardless of the display's refresh rate.

This is a permanent subscription (see 'Main.app'), not started and stopped
around each rotation: 'rAFSubElapsed' keeps re-scheduling its frame callback,
so tearing the subscription down mid-game frees a callback that the browser
has already queued, which crashes the app. Left always-on, it only touches
the model when a tick fires and 'SpinTick' no-ops while no spin is in flight.
-}
spinSub :: Sub Model Action
spinSub = rAFSubElapsed spinIntervalMs SpinTick

{- | Animation-frame tick interval for the rotation animation, in
milliseconds (~60fps). 'rAFSubElapsed' throttles the frame loop to this rate,
and the 'SpinTick' handler advances the spin by the matching fraction of a
turn, so the two stay in step.
-}
spinIntervalMs :: Double
spinIntervalMs = 16

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
    SpinTick -> do
        m <- use this
        -- no-op between rotations; the subscription is always running
        for_ (_spin m) $ \sp -> do
            let step = (spinIntervalMs / 1000) / spinDuration (setupSpeed (_setup m))
            if spinT sp + step >= 1
                then spin .= Nothing
                else spin .= Just sp{spinT = spinT sp + step}
    KeyDown (code, isRepeat, key) ->
        unless isRepeat (handleKey code key)
    NewPiece i -> do
        m <- use this
        let ps = spawnable (_setup m)
            proto = ps !! min i (length ps - 1)
            cells = spawnCells (_setup m) proto
        spin .= Nothing
        pendingLock .= False
        if fits (_setup m) (wellSet m) cells
            then do
                piece .= cells
                -- the hint is for practising only, never for a scored game
                target .= if _practice m then targetPlacement (_setup m) (_well m) cells else []
                ticks .= 0
            else do
                piece .= []
                target .= []
                status .= Over
    Activate item -> runMenu item
    PickLevel n -> pickLevel n
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
    target .= []
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

gotoSetup :: Effect parent props Model Action
gotoSetup = do
    s <- use setup
    scene .= SetupScene PredefRow s

pickLevel :: Int -> Effect parent props Model Action
pickLevel n = do
    startLevel .= n
    startGame False

abortToMenu :: Effect parent props Model Action
abortToMenu = resetGame >> gotoMenu

runMenu :: MenuItem -> Effect parent props Model Action
runMenu = \case
    MenuStart -> do
        n <- use startLevel
        scene .= LevelScene n
    MenuSetup -> gotoSetup
    MenuWrite -> do
        s <- use setup
        io_ (setLocalStorage setupKey (encodeSetup s))
    MenuPractice -> startGame True
    MenuHelp -> scene .= HelpScene

runFame :: FameItem -> Effect parent props Model Action
runFame = \case
    FameStart -> startGame False
    FameSetup -> resetGame >> gotoSetup
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
        _ | code == KeyF -> io_ requestFullscreen
        MenuScene item -> menuKey item code
        SetupScene i draft -> setupSceneKey i draft code
        LevelScene n -> levelKey n code
        GameScene -> gameKey m code
        FameScene item -> fameSceneKey item code

menuKey :: MenuItem -> Int -> Effect parent props Model Action
menuKey item = \case
    ArrowUp -> scene .= MenuScene (cycleEnum (-1) item)
    ArrowDown -> scene .= MenuScene (cycleEnum 1 item)
    KeyEnter -> runMenu item
    -- first-letter shortcuts, as in the original menus
    KeyS -> runMenu MenuStart
    KeyC -> runMenu MenuSetup
    KeyW -> runMenu MenuWrite
    KeyP -> runMenu MenuPractice
    KeyH -> runMenu MenuHelp
    _ -> pure ()

levelKey :: Int -> Int -> Effect parent props Model Action
levelKey n code = case code of
    (ArrowLeft; ArrowUp) -> move (-1)
    (ArrowRight; ArrowDown) -> move 1
    KeyEnter -> pickLevel n
    KeyEsc -> gotoMenu
    _ -> for_ (digitValue code) pickLevel
  where
    move d = scene .= LevelScene ((n + d) `mod` 10)

setupSceneKey :: SetupRow -> Setup -> Int -> Effect parent props Model Action
setupSceneKey row draft = \case
    ArrowUp -> scene .= SetupScene (cycleEnum (-1) row) draft
    ArrowDown -> scene .= SetupScene (cycleEnum 1 row) draft
    ArrowLeft -> change (-1)
    ArrowRight -> change 1
    KeyEnter -> case rowButton row of
        Just b -> setupButton b draft
        Nothing -> change 1
    KeyEsc -> scene .= MenuScene MenuSetup -- cancel, discarding the draft
    _ -> pure ()
  where
    change d = scene .= SetupScene row (adjustRow row d draft)

fameSceneKey :: FameItem -> Int -> Effect parent props Model Action
fameSceneKey item = \case
    ArrowUp -> scene .= FameScene (cycleEnum (-1) item)
    ArrowDown -> scene .= FameScene (cycleEnum 1 item)
    KeyEnter -> runFame item
    KeyS -> runFame FameStart
    KeyC -> runFame FameSetup
    KeyM -> runFame FameMenu
    KeyEsc -> abortToMenu
    _ -> pure ()

nameKey :: MisoString -> Int -> MisoString -> Effect parent props Model Action
nameKey name code key = case code of
    KeyEnter -> do
        m <- use this
        let entries =
                take famePlaces $
                    insertBy (comparing (Down . snd)) (name, _score m) (_fame m)
        fame .= entries
        io_ (setLocalStorage (fameKey (_setup m)) (encodeFame entries))
        scene .= FameScene FameStart
    KeyEsc -> scene .= FameScene FameStart
    KeyBackspace -> scene .= NameScene (ms (dropLast (fromMisoString name)))
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
        KeyEnter | not (_practice m) -> finishGame
        KeyEsc -> abortToMenu
        _ -> pure ()
    Paused -> case code of
        KeyP -> status .= Playing
        KeyEsc -> abortToMenu
        _ -> pure ()
    Playing -> case code of
        -- move: arrows, numpad 4/6/8/2 (NumLock on) and the digit row
        (ArrowLeft; Numpad 4; Digit 4) -> tryMove (-1) 0
        (ArrowRight; Numpad 6; Digit 6) -> tryMove 1 0
        (ArrowUp; Numpad 8; Digit 8) -> tryMove 0 (-1)
        (ArrowDown; Numpad 2; Digit 2) -> tryMove 0 1
        -- diagonals: numpad 7/9/1/3, Home/PgUp/End/PgDn and the digit row
        (Numpad 7; KeyHome; Digit 7) -> tryMove (-1) (-1)
        (Numpad 9; KeyPgUp; Digit 9) -> tryMove 1 (-1)
        (Numpad 1; KeyEnd; Digit 1) -> tryMove (-1) 1
        (Numpad 3; KeyPgDn; Digit 3) -> tryMove 1 1
        KeySpace -> hardDrop
        -- Q/W/E counter-clockwise, A/S/D clockwise about X/Y/Z (manual p.9);
        -- the Q/A and W/S pairs are flipped here so the on-screen turn
        -- matches the original game
        KeyQ -> tryRotate X CW
        KeyA -> tryRotate X CCW
        KeyW -> tryRotate Y CW
        KeyS -> tryRotate Y CCW
        KeyE -> tryRotate Z CCW
        KeyD -> tryRotate Z CW
        KeyP -> status .= Paused
        KeyEsc -> abortToMenu
        _ -> pure ()

-----------------------------------------------------------------------------
-- Gameplay
-----------------------------------------------------------------------------

-- | The locked cubes, as a set for the collision checks.
wellSet :: Model -> Set.Set Cell
wellSet = Set.fromList . _well

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
            then -- the post-drop slide window (manual p.10 note)
                everyTicks lockTicks slamLock
            else -- practice mode: blocks do not descend automatically
                unless (_practice m) (everyTicks (dropTicks (level m)) stepDown)
  where
    inPlay m =
        _scene m == GameScene
            && _status m == Playing
            && not (null (_piece m))

-- | Count a tick, and run the action (resetting the count) every @n@ ticks.
everyTicks :: Int -> Effect parent props Model Action -> Effect parent props Model Action
everyTicks n act = do
    t <- (+ 1) <$> use ticks
    if t >= n
        then ticks .= 0 >> act
        else ticks .= t

tryMove :: Int -> Int -> Effect parent props Model Action
tryMove dx dy = do
    m <- use this
    for_ (movePiece (_setup m) (wellSet m) dx dy (_piece m)) (piece .=)

-- | How far the piece can still fall.
maxDescent :: Model -> Int
maxDescent m = dropDistance (_setup m) (wellSet m) (_piece m)

stepDown :: Effect parent props Model Action
stepDown = do
    m <- use this
    let moved = down 1 (_piece m)
    if fits (_setup m) (wellSet m) moved
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
            then slamLock
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
        (w1, n) = clearLayers s (_piece m ++ _well m)
        lvl = level m
        pf = pitFactor s
        pieceScore = length (_piece m) * (lvl + 1) * setWeight (setupSet s) * pf
        layerScore = 100 * (lvl + 1) * n * n * pf
        -- emptying the whole pit earns a big bonus (manual p.12)
        clearBonus
            | n > 0 && null w1 = 1000 * (lvl + 1) * pf
            | otherwise = 0
    well .= w1
    piece .= []
    target .= []
    spin .= Nothing
    pendingLock .= False
    cubes += length (_piece m)
    cleared += n
    score += pieceScore + layerScore + clearBonus
    spawnPiece

{- | Attempt a rotation of the falling piece (see 'rotatePiece'), and
animate it if it succeeds.
-}
tryRotate :: Axis -> Turn -> Effect parent props Model Action
tryRotate axis turn = do
    m <- use this
    for_ (rotatePiece (_setup m) (wellSet m) axis turn (_piece m)) $ \good -> do
        let (px, py, pz) = centroid (_piece m)
            (gx, gy, gz) = centroid good
        piece .= good
        spin .= Just (Spin axis turn (px - gx, py - gy, pz - gz) 0)
