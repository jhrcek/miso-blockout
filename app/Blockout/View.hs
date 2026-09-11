{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OrPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Rendering: the pit (SVG perspective projection) and the menu screens.
module Blockout.View
    ( viewModel
    , sheet
    ) where

import Data.Foldable (toList)
import Data.List (nub)
import Miso hiding (status, (!!))
import Miso.CSS (StyleSheet)
import qualified Miso.CSS as CSS
import Miso.Html.Element as H
import Miso.Html.Event (onClick)
import Miso.Html.Property as P
import qualified Miso.Svg.Element as S
import qualified Miso.Svg.Property as SP

import Blockout.Types

-----------------------------------------------------------------------------
-- Scene dispatch
-----------------------------------------------------------------------------

viewModel :: ctx -> props -> Model -> View ctx Model Action
viewModel _ _ m =
    H.div_
        [P.class_ "blockout"]
        [ case _scene m of
            GameScene -> gameLayout m
            MenuScene item -> menuView item
            SetupScene i draft -> setupView i draft
            LevelScene n -> levelView n
            HelpScene -> helpView
            NameScene name -> nameView m name
            FameScene item -> fameView m item
        , H.div_ [P.class_ "controls"] [text (footerText m)]
        ]

footerText :: Model -> MisoString
footerText m = case _scene m of
    GameScene -> case _status m of
        Over
            | _practice m -> "ESC \x2500 back to the menu"
            | otherwise -> "ENTER \x2500 hall of fame   \x2502   ESC \x2500 menu"
        Paused -> "P \x2500 resume   \x2502   ESC \x2500 abort game"
        Playing ->
            "\x2190\x2192\x2191\x2193 move   \x2502   Q/A  W/S  E/D rotate   \x2502   SPACE drop   \x2502   P pause   \x2502   ESC menu"
    MenuScene{} -> "\x2191\x2193 select   \x2502   ENTER confirm   \x2502   or press the first letter   \x2502   F fullscreen"
    SetupScene{} -> "\x2191\x2193 row   \x2502   \x2190\x2192 / ENTER change   \x2502   ESC cancel"
    LevelScene{} -> "0-9 pick   \x2502   \x2191\x2193 + ENTER   \x2502   ESC back"
    HelpScene -> "press any key to return"
    NameScene{} -> "type your name   \x2502   ENTER save   \x2502   ESC skip"
    FameScene{} -> "\x2191\x2193 select   \x2502   ENTER confirm   \x2502   ESC menu"

{- | The BLOCK OUT logo of the original: two stacked lines of chunky
slanted letters in a blue box. The "big" variant heads the menu screens.
-}
logo :: Bool -> View ctx Model Action
logo big =
    H.div_
        [P.class_ (if big then "logo big" else "logo")]
        [ H.div_ [P.class_ "logo-block"] ["BLOCK"]
        , H.div_ [P.class_ "logo-out"] ["OUT"]
        ]

-----------------------------------------------------------------------------
-- Menu screens
-----------------------------------------------------------------------------

menuScreen :: MisoString -> [View ctx Model Action] -> View ctx Model Action
menuScreen heading contents =
    H.div_
        [P.class_ "menu"]
        [ logo True
        , H.div_
            [P.class_ "menuScreen"]
            (H.div_ [P.class_ "menuScreen-title"] [text heading] : contents)
        ]

-- | A base class with the "sel" modifier appended when the row is selected.
selClass :: MisoString -> Bool -> MisoString
selClass base sel = if sel then base <> " sel" else base

selectable :: Bool -> Action -> MisoString -> View ctx Model Action
selectable isSel act label =
    H.div_
        [ P.class_ (selClass "mrow" isSel)
        , onClick act
        ]
        [ H.span_ [P.class_ "marker"] [text (if isSel then "\x25BA" else "")]
        , text label
        , H.span_ [P.class_ "marker"] [text (if isSel then "\x25C4" else "")]
        ]

menuView :: MenuItem -> View ctx Model Action
menuView item =
    menuScreen
        "MAIN MENU"
        [ selectable (item == it) (Activate it) label
        | (it, label) <-
            [ (MenuStart, "START GAME")
            , (MenuSetup, "CHOOSE SETUP")
            , (MenuWrite, "WRITE SETUP")
            , (MenuPractice, "PRACTICE MODE")
            , (MenuHelp, "HELP")
            ]
        ]

levelView :: Int -> View ctx Model Action
levelView n =
    menuScreen
        "STARTING LEVEL"
        [ H.div_
            [P.class_ "levels"]
            [ H.div_
                [ P.class_ (selClass "lvl" (k == n))
                , onClick (PickLevel k)
                ]
                [text (ms k)]
            | k <- [0 .. 9]
            ]
        ]

setupView :: Int -> Setup -> View ctx Model Action
setupView i draft =
    menuScreen "CHOOSE SETUP" (map valueRow [0 .. 5] ++ map buttonRow buttons)
  where
    valueRow r =
        H.div_
            [ P.class_ (selClass "srow" (i == r))
            , onClick (SetupClick r (adjustRow r 1 draft))
            ]
            [ H.span_ [P.class_ "slabel"] [text (rowLabel r)]
            , H.span_
                [P.class_ "svalue"]
                [ H.span_ [P.class_ "arrow"] ["\x25C4"]
                , text (rowValue r)
                , H.span_ [P.class_ "arrow"] ["\x25BA"]
                ]
            ]
    rowLabel :: Int -> MisoString
    rowLabel = \case
        0 -> "PREDEFINED SETUP"
        1 -> "BLOCK SET"
        2 -> "ROTATION SPEED"
        3 -> "PIT WIDTH"
        4 -> "PIT LENGTH"
        _ -> "PIT DEPTH"
    rowValue :: Int -> MisoString
    rowValue = \case
        0 -> predefName draft
        1 -> blockSetName (setupSet draft)
        2 -> speedName (setupSpeed draft)
        3 -> ms (setupW draft)
        4 -> ms (setupL draft)
        _ -> ms (setupD draft)
    buttons =
        [ (6, StartB, "START GAME")
        , (7, WriteB, "WRITE SETUP")
        , (8, MenuB, "MAIN MENU")
        ]
    buttonRow (r, b, label) =
        selectable (i == r) (SetupCommit b draft) label

predefName :: Setup -> MisoString
predefName s = case [name | (name, p) <- toList predefined, p == s] of
    (name : _) -> name
    [] -> "CUSTOM"

helpView :: View ctx Model Action
helpView =
    menuScreen "HELP" $
        [ helpRow keys what
        | (keys, what) <-
            [ ("\x2190 \x2192 \x2191 \x2193  or numpad", "move the block")
            , ("numpad 7 9 1 3", "move diagonally")
            , ("Q / A", "flip about the X axis")
            , ("W / S", "turn about the Y axis")
            , ("E / D", "spin about the Z axis")
            , ("SPACE", "drop the block")
            , ("P", "pause / resume")
            , ("F", "fullscreen (ESC to exit)")
            , ("ESC", "abort game, leave menu")
            ]
        ]
            ++ [ H.div_
                    [P.class_ "note"]
                    ["Fill a layer with cubes to clear it. The game ends when the stack reaches the top of the pit."]
               ]
  where
    helpRow keys what =
        H.div_
            [P.class_ "srow"]
            [ H.span_ [P.class_ "slabel"] [text keys]
            , H.span_ [P.class_ "svalue"] [text what]
            ]

nameView :: Model -> MisoString -> View ctx Model Action
nameView m name =
    menuScreen
        "HALL OF FAME"
        [ H.div_ [P.class_ "note bright"] ["YOU MADE THE TOP TEN!"]
        , H.div_ [P.class_ "note"] [text ("YOUR SCORE: " <> ms (_score m))]
        , H.div_ [P.class_ "note"] ["ENTER YOUR NAME:"]
        , H.div_ [P.class_ "name-entry"] [text (name <> "\x2588")]
        ]

fameView :: Model -> FameItem -> View ctx Model Action
fameView m item =
    menuScreen "HALL OF FAME" $
        [ H.div_ [P.class_ "note"] [text (setupCaption (_setup m))]
        ]
            ++ [ H.div_ [P.class_ "note bright"] [text ("YOUR SCORE: " <> ms (_score m))]
               | _score m > 0
               ]
            ++ [ H.div_
                    [P.class_ "fame-table"]
                    [ H.div_
                        [P.class_ "frow"]
                        [ H.span_ [P.class_ "frank"] [text (ms (k :: Int) <> ".")]
                        , H.span_ [P.class_ "fname"] [text name]
                        , H.span_ [P.class_ "fscore"] [text scoreTxt]
                        ]
                    | (k, (name, scoreTxt)) <- zip [1 ..] rows
                    ]
               ]
            ++ [ selectable (item == it) (FameActivate it) label
               | (it, label) <-
                    [ (FameStart, "START GAME")
                    , (FameSetup, "CHOOSE SETUP")
                    , (FameMenu, "MAIN MENU")
                    ]
               ]
  where
    rows =
        [ (name, ms sc)
        | (name, sc) <- _fame m
        ]
            ++ replicate (famePlaces - length (_fame m)) ("\x2026", "")

setupCaption :: Setup -> MisoString
setupCaption s =
    ms (setupW s)
        <> "\x00D7"
        <> ms (setupL s)
        <> "\x00D7"
        <> ms (setupD s)
        <> " \x2022 "
        <> blockSetName (setupSet s)
        <> " SET"

-----------------------------------------------------------------------------
-- Game screen
-----------------------------------------------------------------------------

gameLayout :: Model -> View ctx Model Action
gameLayout m =
    H.div_
        [P.class_ "layout"]
        [ leftPanel m
        , pitSvg m
        , rightPanel m
        ]

pitSvg :: Model -> View ctx Model Action
pitSvg m =
    S.svg_
        [ P.width_ (ms pitPx)
        , P.height_ (ms pitPx)
        , SP.viewBox_ ("0 0 " <> ms pitPx <> " " <> ms pitPx)
        , P.class_ "pit"
        ]
        ( pitGrid s
            -- When paused, hide the well and the falling piece so players
            -- cannot study the position while the game is frozen.
            ++ ( if _status m == Paused
                    then [banner "PAUSED"]
                    else wellCubes s (_well m) ++ pieceWire s (_spin m) (_piece m)
               )
        )
  where
    s = _setup m
    banner t =
        S.text_
            [ SP.x_ (msd halfSize)
            , SP.y_ (msd (halfSize + 16))
            , SP.textAnchor_ "middle"
            , SP.fill_ "#ffff55"
            , SP.fontSize_ "40"
            , SP.fontWeight_ "700"
            , SP.fontFamily_ "'Chakra Petch', sans-serif"
            , CSS.style_ ["letter-spacing" =: "6px"]
            ]
            [text t]

{- | Left column, as in the original: the level and the pit depth indicator,
a strip that fills from the bottom with the colour of every layer that
holds at least one cube.
-}
leftPanel :: Model -> View ctx Model Action
leftPanel m =
    H.div_
        [P.class_ "panel"]
        [ infoBox "LEVEL" (ms (level m))
        , H.div_
            [P.class_ "stack"]
            [ H.div_
                [ P.class_ (if filled z then "seg on" else "seg")
                , CSS.style_ ["background-color" =: segColor z]
                ]
                []
            | z <- [0 .. setupD (_setup m) - 1]
            ]
        ]
  where
    filled z = any (\(_, _, cz) -> cz == z) (_well m)
    segColor z
        | filled z = faceColor (setupD (_setup m)) z
        | otherwise = "transparent"

{- | Right column, laid out like the original: the logo, score and cubes
played on top, high score, pit and block set at the bottom. The gap in
between hosts the status box (practice / paused / game over).
-}
rightPanel :: Model -> View ctx Model Action
rightPanel m =
    H.div_
        [P.class_ "panel wide"]
        [ logo False
        , infoBox "SCORE" (ms (_score m))
        , infoBox "CUBES PLAYED" (ms (_cubes m))
        , H.div_ [P.class_ "spacer"] statusBox
        , infoBox "HIGH SCORE" (ms (max (fameBest m) (_score m)))
        , infoBox "PIT" (pitCaption (_setup m))
        , infoBox "BLOCK SET" (blockSetName (setupSet (_setup m)))
        ]
  where
    statusBox = case _status m of
        Over -> [H.div_ [P.class_ "status over"] ["GAME", H.br_ [], "OVER"]]
        Paused -> [H.div_ [P.class_ "status paused"] ["PAUSED"]]
        Playing
            | _practice m -> [H.div_ [P.class_ "status practice"] ["PRACTICE"]]
            | otherwise -> []

pitCaption :: Setup -> MisoString
pitCaption s =
    ms (setupW s) <> "\x00D7" <> ms (setupL s) <> "\x00D7" <> ms (setupD s)

infoBox :: MisoString -> MisoString -> View ctx Model Action
infoBox label val =
    H.div_
        [P.class_ "infobox"]
        [ H.div_ [P.class_ "label"] [text label]
        , H.div_ [P.class_ "value"] [text val]
        ]

-----------------------------------------------------------------------------
-- Perspective projection into the pit
-----------------------------------------------------------------------------

-- | Side of the square pit drawing, in CSS pixels.
pitPx :: Int
pitPx = 600

halfSize :: Double
halfSize = fi pitPx / 2

fi :: Int -> Double
fi = fromIntegral

{- | Project a pit coordinate to SVG screen space. The eye looks straight
down the Z axis through the center of the pit mouth.
-}
proj :: Setup -> Double -> Double -> Double -> (Double, Double)
proj s x y z =
    ( halfSize + (x - cx) * unit * k
    , halfSize + (y - cy) * unit * k
    )
  where
    cx = fi (setupW s) / 2
    cy = fi (setupL s) / 2
    -- the pit mouth spans the whole drawing (1px inset keeps the outer
    -- ring's stroke from being clipped)
    unit = (2 * halfSize - 2) / fi (max (setupW s) (setupL s))
    f = focalOf s
    k = f / (f + z)

-- | Eye distance from the pit mouth, scaled so deep pits stay legible.
focalOf :: Setup -> Double
focalOf s = max 3 (fi (setupD s) * 5 / 12)

msd :: Double -> MisoString
msd d = ms (fromIntegral (round (d * 10) :: Int) / 10 :: Double)

pointsOf :: [(Double, Double)] -> MisoString
pointsOf ps = ms (unwords [pt p | p <- ps])
  where
    pt (a, b) = fromMisoString (msd a) <> "," <> fromMisoString (msd b)

poly :: MisoString -> MisoString -> MisoString -> [(Double, Double)] -> View ctx Model Action
poly fillCol strokeCol w ps =
    S.polygon_
        [ SP.points_ (pointsOf ps)
        , SP.fill_ fillCol
        , SP.stroke_ strokeCol
        , SP.strokeWidth_ w
        ]

lineSeg :: MisoString -> MisoString -> (Double, Double) -> (Double, Double) -> View ctx Model Action
lineSeg strokeCol w (ax, ay) (bx, by) =
    S.line_
        [ SP.x1_ (msd ax)
        , SP.y1_ (msd ay)
        , SP.x2_ (msd bx)
        , SP.y2_ (msd by)
        , SP.stroke_ strokeCol
        , SP.strokeWidth_ w
        ]

gridColor :: MisoString
gridColor = "#00aa00"

-- | The green wireframe of the empty pit.
pitGrid :: Setup -> [View ctx Model Action]
pitGrid s =
    concat
        [ [poly "none" gridColor "1" (ring (fi z)) | z <- [0 .. d]]
        , [gline (proj s x y 0) (proj s x y depth) | x <- [0 .. fi w], y <- [0, fi l]]
        , [gline (proj s x y 0) (proj s x y depth) | x <- [0, fi w], y <- [1 .. fi l - 1]]
        , [gline (proj s x 0 depth) (proj s x (fi l) depth) | x <- [0 .. fi w]]
        , [gline (proj s 0 y depth) (proj s (fi w) y depth) | y <- [0 .. fi l]]
        ]
  where
    gline = lineSeg gridColor "1"
    w = setupW s
    l = setupL s
    d = setupD s
    depth = fi d
    ring z = [proj s 0 0 z, proj s (fi w) 0 z, proj s (fi w) (fi l) z, proj s 0 (fi l) z]

{- | (face, shaded side) color per pit layer, matching the original game's
cycle. Colors are anchored to the bottom of the pit and run toward the
viewer, repeating every 7 layers.
-}
palette :: [(MisoString, MisoString)]
palette =
    [ ("#0000aa", "#000055")
    , ("#00aa00", "#005500")
    , ("#00aaaa", "#005555")
    , ("#aa0000", "#550000")
    , ("#aa00aa", "#550055")
    , ("#aa5500", "#552a00")
    , ("#aaaaaa", "#555555")
    ]

{- | Index into the cycle for layer @z@ in a pit of depth @d@, counting from
the bottom-most layer (z = d - 1) toward the mouth (z = 0).
-}
paletteIx :: Int -> Int -> Int
paletteIx d z = (d - 1 - z) `mod` length palette

faceColor, sideColor :: Int -> Int -> MisoString
faceColor d z = fst (palette !! paletteIx d z)
sideColor d z = snd (palette !! paletteIx d z)

{- | Locked cubes, painted back to front. Each cube shows its front face,
plus any side faces that look toward the viewer and are not hidden by a
neighbouring cube in the same layer.
-}
wellCubes :: Setup -> [Cell] -> [View ctx Model Action]
wellCubes s w = concat [layerViews z | z <- [setupD s - 1, setupD s - 2 .. 0]]
  where
    cx = fi (setupW s) / 2
    cy = fi (setupL s) / 2
    layerViews z =
        let cs = [c | c@(_, _, cz) <- w, cz == z]
         in concatMap sideFaces cs ++ map frontFace cs
    frontFace (x, y, z) =
        poly
            (faceColor (setupD s) z)
            "#000000"
            "1"
            [pr x y z, pr (x + 1) y z, pr (x + 1) (y + 1) z, pr x (y + 1) z]
    sideFaces (x, y, z) =
        concat
            [ [ quad z [pr x y z, pr x (y + 1) z, pr x (y + 1) (z + 1), pr x y (z + 1)]
              | fi x > cx
              , free (x - 1) y z
              ]
            , [ quad z [pr (x + 1) y z, pr (x + 1) (y + 1) z, pr (x + 1) (y + 1) (z + 1), pr (x + 1) y (z + 1)]
              | fi (x + 1) < cx
              , free (x + 1) y z
              ]
            , [ quad z [pr x y z, pr (x + 1) y z, pr (x + 1) y (z + 1), pr x y (z + 1)]
              | fi y > cy
              , free x (y - 1) z
              ]
            , [ quad z [pr x (y + 1) z, pr (x + 1) (y + 1) z, pr (x + 1) (y + 1) (z + 1), pr x (y + 1) (z + 1)]
              | fi (y + 1) < cy
              , free x (y + 1) z
              ]
            ]
    quad z = poly (sideColor (setupD s) z) "#000000" "1"
    free x y z = (x, y, z) `notElem` w
    pr x y z = proj s (fi x) (fi y) (fi z)

{- | The falling piece, drawn as a white wireframe of its outline only.
While a rotation animation is in flight, every outline corner is rotated
back by the not-yet-elapsed part of the 90 degree turn about the piece
centroid (and translated back along any wall-kick offset), so the
wireframe sweeps smoothly into its final resting orientation.
-}
pieceWire :: Setup -> Maybe Spin -> [Cell] -> [View ctx Model Action]
pieceWire s msp cs =
    [lineSeg "#ffffff" "1.5" (corner a) (corner b) | (a, b) <- outlineEdges cs]
  where
    corner (x, y, z) = let (px, py, pz) = place (fi x) (fi y) (fi z) in proj s px py pz
    place = case msp of
        Nothing -> (,,)
        Just (Spin axis dir (ox, oy, oz) t) ->
            let theta = -(dir * (pi / 2) * (1 - t))
                (cx0, cy0, cz0) = centroid cs
             in \x y z ->
                    let (rx, ry, rz) = rotate3 axis theta (x - cx0, y - cy0, z - cz0)
                     in ( rx + cx0 + (1 - t) * ox
                        , ry + cy0 + (1 - t) * oy
                        , rz + cz0 + (1 - t) * oz
                        )

{- | Rotate a vector by @theta@ radians about the X, Y or Z axis. At
+90 degrees this agrees with the linear part of the corresponding
discrete cw rotation, at -90 degrees with the ccw one.
-}
rotate3 :: Int -> Double -> (Double, Double, Double) -> (Double, Double, Double)
rotate3 0 th (x, y, z) = (x, y * cos th - z * sin th, y * sin th + z * cos th)
rotate3 1 th (x, y, z) = (x * cos th - z * sin th, y, x * sin th + z * cos th)
rotate3 _ th (x, y, z) = (x * cos th - y * sin th, x * sin th + y * cos th, z)

{- | The crease edges of the union of the piece's unit cubes, in lattice
corner coordinates. Each lattice edge touches up to four cells; it is
part of the outline when 1 or 3 of them are filled, or when exactly 2
are filled diagonally. Edges in the middle of a flat surface (2 filled
side by side) or interior edges (0 or 4 filled) are not drawn.
-}
outlineEdges :: [Cell] -> [(Cell, Cell)]
outlineEdges cs =
    concat
        [ [ ((x, y, z), (x + 1, y, z))
          | (x, y, z) <- nub [(cx, cy + dy, cz + dz) | (cx, cy, cz) <- cs, dy <- [0, 1], dz <- [0, 1]]
          , sharp (occ (x, y - 1, z - 1)) (occ (x, y - 1, z)) (occ (x, y, z - 1)) (occ (x, y, z))
          ]
        , [ ((x, y, z), (x, y + 1, z))
          | (x, y, z) <- nub [(cx + dx, cy, cz + dz) | (cx, cy, cz) <- cs, dx <- [0, 1], dz <- [0, 1]]
          , sharp (occ (x - 1, y, z - 1)) (occ (x - 1, y, z)) (occ (x, y, z - 1)) (occ (x, y, z))
          ]
        , [ ((x, y, z), (x, y, z + 1))
          | (x, y, z) <- nub [(cx + dx, cy + dy, cz) | (cx, cy, cz) <- cs, dx <- [0, 1], dy <- [0, 1]]
          , sharp (occ (x - 1, y - 1, z)) (occ (x - 1, y, z)) (occ (x, y - 1, z)) (occ (x, y, z))
          ]
        ]
  where
    occ c = c `elem` cs
    -- a\/d and b\/c are the diagonal pairs of the four cells around an edge
    sharp a b c d = case length (filter id [a, b, c, d]) of
        (1; 3) -> True
        2 -> (a && d) || (b && c)
        _ -> False

-----------------------------------------------------------------------------
-- Stylesheet. The palette is the original's EGA one: blue boxes, cyan
-- labels, yellow values, green pit, white menu text on black.
-----------------------------------------------------------------------------

-- | Natural (unscaled) size of the whole layout, in CSS pixels.
layoutW, layoutH :: Int
layoutW = pitPx + 2 * 16 + 100 + 220 -- pit, gaps, left and right columns
layoutH = pitPx + 8 + 26 -- pit, gap, footer

blue, brightBlue, cyan, yellow, white, grey, darkGrey, red :: MisoString
blue = "#0000aa"
brightBlue = "#5555ff"
cyan = "#55ffff"
yellow = "#ffff55"
white = "#ffffff"
grey = "#aaaaaa"
darkGrey = "#555555"
red = "#ff5555"

{- | A smooth squarish "tech" face that keeps the arcade feel; sans-serif
fallback while it loads.
-}
uiFont :: MisoString
uiFont = "'Chakra Petch', 'Trebuchet MS', 'DejaVu Sans', sans-serif"

-- | The chunky slanted face of the logo.
logoFont :: MisoString
logoFont = "'Arial Black', Impact, 'Helvetica Neue', Arial, sans-serif"

{- | A two-tone bevelled frame: the original draws its boxes in blue with a
black inner line; the faint outer glow is the modern touch.
-}
boxFrame :: MisoString -> [CSS.Style]
boxFrame col =
    [ "border" =: ("3px solid " <> col)
    , "box-shadow" =: ("inset 0 0 0 1px #000000, inset 0 0 0 2px " <> col <> "40, 0 0 14px " <> col <> "33")
    , "background-color" =: "#000000"
    ]

sheet :: StyleSheet
sheet =
    CSS.sheet_
        [ CSS.selector_
            "html, body"
            [ CSS.margin "0"
            , CSS.height "100%"
            , "overflow" =: "hidden"
            , "background-color" =: "#000000"
            ]
        , CSS.selector_
            "body"
            [ CSS.display "flex"
            , CSS.justifyContent "center"
            , CSS.alignItems "center"
            , CSS.fontFamily uiFont
            , CSS.fontWeight "600"
            , "color" =: white
            , "-webkit-font-smoothing" =: "antialiased"
            , -- the UI is keyboard/click driven, so suppress text selection
              -- (e.g. when mashing keys or clicking menu rows)
              CSS.userSelect "none"
            ]
        , CSS.selector_
            ".blockout"
            [ CSS.display "flex"
            , "flex-direction" =: "column"
            , "gap" =: "8px"
            , "align-items" =: "stretch"
            , "width" =: px layoutW
            , "height" =: px layoutH
            , -- Scale the whole UI up by the largest factor that still fits the
              -- viewport. min() picks the binding dimension, so aspect ratio is
              -- preserved; the body's flex-center keeps it centred and its
              -- overflow:hidden suppresses scrollbars.
              "transform" =: ("scale(min(100vw / " <> px layoutW <> ", 100vh / " <> px layoutH <> "))")
            , "transform-origin" =: "center center"
            ]
        , -- logo
          CSS.selector_
            ".logo"
            ( boxFrame blue
                ++ [ CSS.fontFamily logoFont
                   , CSS.fontWeight "900"
                   , "font-style" =: "italic"
                   , CSS.textAlign "center"
                   , "line-height" =: "1"
                   , "letter-spacing" =: "1px"
                   , CSS.padding (CSS.px 10)
                   , CSS.display "flex"
                   , "flex-direction" =: "column"
                   , "align-items" =: "center"
                   , "gap" =: "4px"
                   ]
            )
        , CSS.selector_
            ".logo-block"
            [ "color" =: white
            , CSS.fontSize "34px"
            , "text-shadow" =: ("2px 2px 0 " <> blue <> ", 4px 4px 0 #000066")
            ]
        , CSS.selector_
            ".logo-out"
            [ "color" =: red
            , "background-color" =: blue
            , CSS.fontSize "26px"
            , CSS.padding "0 10px 2px 8px"
            , "-webkit-text-stroke" =: "0.6px #ffffff"
            , "text-shadow" =: "2px 2px 0 #000066"
            ]
        , CSS.selector_
            ".logo.big"
            [ "align-self" =: "center"
            , CSS.padding "8px 36px"
            , "margin-bottom" =: "12px"
            ]
        , CSS.selector_ ".logo.big .logo-block" [CSS.fontSize "44px"]
        , CSS.selector_ ".logo.big .logo-out" [CSS.fontSize "32px", CSS.padding "0 14px 3px 12px"]
        , -- game screen
          CSS.selector_
            ".layout"
            [ CSS.display "flex"
            , "flex-direction" =: "row"
            , "gap" =: "16px"
            , "align-items" =: "stretch"
            , "height" =: px pitPx
            ]
        , CSS.selector_
            ".pit"
            [ "background-color" =: "#000000"
            , "flex" =: "none"
            ]
        , CSS.selector_
            ".panel"
            [ CSS.display "flex"
            , "flex-direction" =: "column"
            , "gap" =: "12px"
            , "width" =: "100px"
            , "flex" =: "none"
            ]
        , CSS.selector_
            ".panel.wide"
            [ "width" =: "220px"
            ]
        , CSS.selector_
            ".spacer"
            [ "flex" =: "1"
            , CSS.display "flex"
            , "align-items" =: "center"
            , "justify-content" =: "center"
            ]
        , CSS.selector_
            ".status"
            ( boxFrame red
                ++ [ CSS.fontSize "24px"
                   , "line-height" =: "1"
                   , "color" =: yellow
                   , CSS.textAlign "center"
                   , CSS.padding "10px 18px"
                   , "letter-spacing" =: "2px"
                   , "width" =: "100%"
                   , "box-sizing" =: "border-box"
                   ]
            )
        , CSS.selector_ ".status.paused" (boxFrame brightBlue)
        , CSS.selector_ ".status.practice" (boxFrame "#00aa00" ++ ["color" =: "#55ff55", CSS.fontSize "20px"])
        , CSS.selector_
            ".stack"
            ( boxFrame blue
                ++ [ "flex" =: "1"
                   , CSS.display "flex"
                   , "flex-direction" =: "column"
                   , "gap" =: "3px"
                   , CSS.padding (CSS.px 6)
                   ]
            )
        , CSS.selector_
            ".seg"
            [ "flex" =: "1"
            , "border-radius" =: "1px"
            , "transition" =: "background-color 0.15s ease-out"
            ]
        , CSS.selector_
            ".seg.on"
            [ "box-shadow" =: "inset 0 0 0 1px rgba(255,255,255,0.25)"
            ]
        , CSS.selector_
            ".infobox .label"
            [ "color" =: cyan
            , CSS.fontSize "14px"
            , "line-height" =: "1"
            , CSS.textAlign "center"
            , "margin-bottom" =: "4px"
            , "letter-spacing" =: "1px"
            ]
        , CSS.selector_
            ".infobox .value"
            ( boxFrame blue
                ++ [ "color" =: yellow
                   , CSS.fontSize "24px"
                   , CSS.fontWeight "700"
                   , "line-height" =: "1"
                   , CSS.textAlign "center"
                   , CSS.padding "7px 8px 5px"
                   , "letter-spacing" =: "1px"
                   , "white-space" =: "nowrap"
                   , "overflow" =: "hidden"
                   ]
            )
        , CSS.selector_
            ".controls"
            [ "color" =: darkGrey
            , CSS.fontSize "14px"
            , CSS.fontWeight "500"
            , "line-height" =: "26px"
            , CSS.textAlign "center"
            , "white-space" =: "nowrap"
            , "overflow" =: "hidden"
            ]
        , -- menu screens
          CSS.selector_
            ".menu"
            [ "flex" =: "1"
            , CSS.display "flex"
            , "flex-direction" =: "column"
            , "align-items" =: "center"
            , "justify-content" =: "center"
            ]
        , CSS.selector_
            ".menuScreen"
            ( boxFrame grey
                ++ [ "width" =: "600px"
                   , "box-sizing" =: "border-box"
                   , CSS.display "flex"
                   , "flex-direction" =: "column"
                   , "gap" =: "2px"
                   , CSS.padding "14px 28px 18px"
                   ]
            )
        , CSS.selector_
            ".menuScreen-title"
            [ "color" =: cyan
            , CSS.fontSize "22px"
            , CSS.fontWeight "700"
            , "line-height" =: "1"
            , CSS.textAlign "center"
            , "letter-spacing" =: "4px"
            , "margin-bottom" =: "10px"
            , "border-bottom" =: ("2px solid " <> darkGrey)
            , "padding-bottom" =: "8px"
            ]
        , CSS.selector_
            ".mrow"
            [ "color" =: white
            , CSS.fontSize "22px"
            , CSS.fontWeight "700"
            , "line-height" =: "1"
            , CSS.padding "8px 12px 6px"
            , CSS.textAlign "center"
            , "letter-spacing" =: "2px"
            , "cursor" =: "pointer"
            , CSS.display "flex"
            , "justify-content" =: "center"
            , "gap" =: "14px"
            , "transition" =: "background-color 0.08s ease-out, color 0.08s ease-out"
            ]
        , CSS.selector_ ".mrow:hover" ["color" =: yellow]
        , CSS.selector_
            ".marker"
            [ "width" =: "18px"
            , CSS.display "inline-block"
            , CSS.textAlign "center"
            , "color" =: yellow
            ]
        , CSS.selector_
            ".srow"
            [ "color" =: white
            , CSS.fontSize "18px"
            , "line-height" =: "1"
            , CSS.padding "7px 12px 5px"
            , CSS.display "flex"
            , "justify-content" =: "space-between"
            , "align-items" =: "baseline"
            , "gap" =: "16px"
            , "cursor" =: "pointer"
            , "transition" =: "background-color 0.08s ease-out"
            ]
        , CSS.selector_
            ".svalue"
            [ "color" =: yellow
            , CSS.display "flex"
            , "gap" =: "10px"
            , "align-items" =: "baseline"
            ]
        , CSS.selector_ ".arrow" ["color" =: darkGrey, CSS.fontSize "13px"]
        , CSS.selector_ ".sel .arrow" ["color" =: cyan]
        , CSS.selector_
            ".sel"
            [ "background-color" =: blue
            , "color" =: yellow
            ]
        , CSS.selector_
            ".note"
            [ "color" =: grey
            , CSS.fontSize "17px"
            , CSS.fontWeight "500"
            , "line-height" =: "1.25"
            , CSS.textAlign "center"
            , CSS.padding "2px 0"
            ]
        , CSS.selector_
            ".note.bright"
            [ "color" =: yellow
            , CSS.fontSize "22px"
            , CSS.fontWeight "700"
            , "letter-spacing" =: "2px"
            ]
        , CSS.selector_
            ".levels"
            [ CSS.display "flex"
            , "flex-direction" =: "column"
            , "align-items" =: "center"
            , "gap" =: "2px"
            ]
        , CSS.selector_
            ".lvl"
            [ "color" =: grey
            , CSS.fontSize "22px"
            , CSS.fontWeight "700"
            , "line-height" =: "1"
            , "width" =: "120px"
            , CSS.textAlign "center"
            , CSS.padding "5px 0 3px"
            , "cursor" =: "pointer"
            , "transition" =: "background-color 0.08s ease-out, color 0.08s ease-out"
            ]
        , CSS.selector_ ".lvl:hover" ["color" =: white]
        , CSS.selector_
            ".lvl.sel"
            [ "color" =: yellow
            , "background-color" =: blue
            ]
        , CSS.selector_
            ".name-entry"
            ( boxFrame blue
                ++ [ "color" =: yellow
                   , CSS.fontSize "28px"
                   , CSS.fontWeight "700"
                   , "line-height" =: "1"
                   , CSS.textAlign "center"
                   , "letter-spacing" =: "3px"
                   , CSS.padding "10px 10px 8px"
                   , "margin" =: "10px 80px"
                   , "min-height" =: "36px"
                   ]
            )
        , -- the ten places run down two columns of five
          CSS.selector_
            ".fame-table"
            [ CSS.display "grid"
            , "grid-template-columns" =: "1fr 1fr"
            , "grid-template-rows" =: "repeat(5, auto)"
            , "grid-auto-flow" =: "column"
            , "column-gap" =: "36px"
            , "border-top" =: ("2px solid " <> darkGrey)
            , "border-bottom" =: ("2px solid " <> darkGrey)
            , CSS.padding "8px 16px"
            , "margin" =: "6px 0 10px"
            ]
        , CSS.selector_
            ".frow"
            [ CSS.display "flex"
            , "gap" =: "14px"
            , "color" =: white
            , CSS.fontSize "17px"
            , "line-height" =: "1"
            , CSS.padding "3px 0"
            ]
        , CSS.selector_
            ".frank"
            [ "width" =: "40px"
            , CSS.textAlign "right"
            , "color" =: grey
            ]
        , CSS.selector_
            ".fname"
            [ "flex" =: "1"
            ]
        , CSS.selector_
            ".fscore"
            [ "color" =: yellow
            ]
        ]
  where
    px :: Int -> MisoString
    px n = ms n <> "px"
