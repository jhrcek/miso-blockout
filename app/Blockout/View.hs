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

viewModel :: props -> Model -> View Model Action
viewModel _ m =
    H.div_
        [P.class_ "blockout"]
        [ H.div_ [P.class_ "titlebar"] ["BLOCKOUT \x2014 \x1F35C miso"]
        , case _scene m of
            GameScene -> gameLayout m
            MenuScene item -> menuView item
            SetupScene i draft -> setupView i draft
            LevelScene n -> levelView n
            HelpScene -> helpView
            NameScene name -> nameView m name
            FameScene item -> fameView m item
        , H.div_ [P.class_ "controls"] [text (footerText (_scene m))]
        ]

footerText :: Scene -> MisoString
footerText = \case
    GameScene ->
        "\x2190 \x2192 \x2191 \x2193 / numpad 1-9 move \x2022 Q/A W/S E/D rotate \x2022 SPACE drop \x2022 P pause \x2022 ESC menu"
    MenuScene{} -> "\x2191 \x2193 select \x2022 ENTER confirm \x2022 or press the first letter \x2022 F fullscreen"
    SetupScene{} -> "\x2191 \x2193 row \x2022 \x2190 \x2192 / ENTER change \x2022 ESC cancel"
    LevelScene{} -> "0-9 pick \x2022 \x2191 \x2193 + ENTER \x2022 ESC back"
    HelpScene -> "press any key to return"
    NameScene{} -> "type your name \x2022 ENTER save \x2022 ESC skip"
    FameScene{} -> "\x2191 \x2193 select \x2022 ENTER confirm \x2022 ESC menu"

-----------------------------------------------------------------------------
-- Menu screens
-----------------------------------------------------------------------------

menuScreen :: MisoString -> [View Model Action] -> View Model Action
menuScreen heading contents =
    H.div_
        [P.class_ "menuScreen"]
        (H.div_ [P.class_ "menuScreen-title"] [text heading] : contents)

-- | A base class with the "sel" modifier appended when the row is selected.
selClass :: MisoString -> Bool -> MisoString
selClass base sel = if sel then base <> " sel" else base

selectable :: Bool -> Action -> MisoString -> View Model Action
selectable isSel act label =
    H.div_
        [ P.class_ (selClass "mrow" isSel)
        , onClick act
        ]
        [text (if isSel then "\x25BA " <> label else label)]

menuView :: MenuItem -> View Model Action
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

levelView :: Int -> View Model Action
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

setupView :: Int -> Setup -> View Model Action
setupView i draft =
    menuScreen "CHOOSE SETUP" (map valueRow [0 .. 5] ++ map buttonRow buttons)
  where
    valueRow r =
        H.div_
            [ P.class_ (selClass "srow" (i == r))
            , onClick (SetupClick r (adjustRow r 1 draft))
            ]
            [ H.span_ [P.class_ "slabel"] [text (rowLabel r)]
            , H.span_ [P.class_ "svalue"] [text ("\x25C4 " <> rowValue r <> " \x25BA")]
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

helpView :: View Model Action
helpView =
    menuScreen "HELP" $
        [ helpRow keys what
        | (keys, what) <-
            [ ("\x2190 \x2192 \x2191 \x2193", "move the block")
            , ("numpad 4 6 8 2", "move the block")
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

nameView :: Model -> MisoString -> View Model Action
nameView m name =
    menuScreen
        "HALL OF FAME"
        [ H.div_ [P.class_ "note bright"] ["YOU MADE THE TOP TEN!"]
        , H.div_ [P.class_ "note"] [text ("YOUR SCORE: " <> ms (_score m))]
        , H.div_ [P.class_ "note"] ["ENTER YOUR NAME:"]
        , H.div_ [P.class_ "name-entry"] [text (name <> "\x2588")]
        ]

fameView :: Model -> FameItem -> View Model Action
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

gameLayout :: Model -> View Model Action
gameLayout m =
    H.div_
        [P.class_ "layout"]
        [ leftPanel m
        , pitSvg m
        , rightPanel m
        ]

pitSvg :: Model -> View Model Action
pitSvg m =
    S.svg_
        [ P.width_ "560"
        , P.height_ "560"
        , SP.viewBox_ "0 0 560 560"
        , P.class_ "pit"
        ]
        ( pitGrid s
            -- When paused, hide the well and the falling piece so players
            -- cannot study the position while the game is frozen.
            ++ ( if _status m == Paused
                    then []
                    else wellCubes s (_well m) ++ pieceWire s (_spin m) (_piece m)
               )
            ++ overlay (_practice m) (_status m)
        )
  where
    s = _setup m

leftPanel :: Model -> View Model Action
leftPanel m =
    H.div_
        [P.class_ "panel"]
        [ infoBox "LEVEL" (ms (level m))
        , H.div_
            [P.class_ "stack"]
            [ H.div_
                [ P.class_ "seg"
                , CSS.style_ ["background-color" =: segColor z]
                ]
                []
            | z <- [0 .. setupD (_setup m) - 1]
            ]
        ]
  where
    segColor z
        | any (\(_, _, cz) -> cz == z) (_well m) = faceColor (setupD (_setup m)) z
        | otherwise = "#101010"

rightPanel :: Model -> View Model Action
rightPanel m =
    H.div_
        [P.class_ "panel wide"]
        ( [infoBox "MODE" "PRACTICE" | _practice m]
            ++ [ infoBox "SCORE" (ms (_score m))
               , infoBox "CUBES PLAYED" (ms (_cubes m))
               , infoBox "LAYERS" (ms (_cleared m))
               , infoBox "HIGH SCORE" (ms (max (fameBest m) (_score m)))
               , infoBox "PIT" (pitCaption (_setup m))
               , infoBox "BLOCK SET" (blockSetName (setupSet (_setup m)))
               ]
        )

pitCaption :: Setup -> MisoString
pitCaption s =
    ms (setupW s) <> "\x00D7" <> ms (setupL s) <> "\x00D7" <> ms (setupD s)

infoBox :: MisoString -> MisoString -> View Model Action
infoBox label val =
    H.div_
        [P.class_ "infobox"]
        [ H.div_ [P.class_ "label"] [text label]
        , H.div_ [P.class_ "value"] [text val]
        ]

-----------------------------------------------------------------------------
-- Perspective projection into the pit
-----------------------------------------------------------------------------
halfSize :: Double
halfSize = 280

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
    unit = 530 / fi (max (setupW s) (setupL s))
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

poly :: MisoString -> MisoString -> MisoString -> [(Double, Double)] -> View Model Action
poly fillCol strokeCol w ps =
    S.polygon_
        [ SP.points_ (pointsOf ps)
        , SP.fill_ fillCol
        , SP.stroke_ strokeCol
        , SP.strokeWidth_ w
        ]

lineSeg :: MisoString -> MisoString -> (Double, Double) -> (Double, Double) -> View Model Action
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
gridColor = "#00b400"

-- | The green wireframe of the empty pit.
pitGrid :: Setup -> [View Model Action]
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
wellCubes :: Setup -> [Cell] -> [View Model Action]
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
pieceWire :: Setup -> Maybe Spin -> [Cell] -> [View Model Action]
pieceWire s msp cs =
    [lineSeg "#ffffff" "1.5" (corner a) (corner b) | (a, b) <- outlineEdges cs]
  where
    corner (x, y, z) = let (px, py, pz) = place (fi x) (fi y) (fi z) in proj s px py pz
    place = case msp of
        Nothing -> (,,)
        Just (Spin axis dir (ox, oy, oz) t _) ->
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

overlay :: Bool -> Status -> [View Model Action]
overlay isPractice = \case
    Playing -> []
    Paused ->
        [shade, banner 296 "40" "#ffd000" "PAUSED"]
    Over
        -- practice mode has no hall of fame; only the menu is offered
        | isPractice ->
            [ shade
            , banner 260 "44" "#ff3030" "GAME OVER"
            , banner 310 "20" "#ffffff" "press ESC for the menu"
            ]
        | otherwise ->
            [ shade
            , banner 260 "44" "#ff3030" "GAME OVER"
            , banner 310 "20" "#ffffff" "press ENTER for the Hall of Fame"
            , banner 340 "20" "#ffffff" "ESC for the menu"
            ]
  where
    shade =
        S.rect_
            [ SP.x_ "0"
            , SP.y_ "0"
            , P.width_ "560"
            , P.height_ "560"
            , SP.fill_ "rgba(0,0,0,0.65)"
            ]
    banner y size col t =
        S.text_
            [ SP.x_ "280"
            , SP.y_ (ms (y :: Int))
            , SP.textAnchor_ "middle"
            , SP.fill_ col
            , SP.fontSize_ size
            , SP.fontFamily_ "'Courier New', monospace"
            , SP.fontWeight_ "bold"
            ]
            [text t]

-----------------------------------------------------------------------------
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
            , CSS.fontFamily "'Courier New', monospace"
            , "color" =: "#00cc00"
            , -- the UI is keyboard/click driven, so suppress text selection
              -- (e.g. when mashing keys or clicking menu rows)
              CSS.userSelect "none"
            ]
        , CSS.selector_
            ".blockout"
            [ CSS.display "flex"
            , "flex-direction" =: "column"
            , "gap" =: "10px"
            , "align-items" =: "stretch"
            , -- Scale the whole UI up by the largest factor that still fits the
              -- viewport. min() picks the binding dimension, so aspect ratio is
              -- preserved; the body's flex-center keeps it centred and its
              -- overflow:hidden suppresses scrollbars. 846x680 is the layout's
              -- natural size (panels + pit, titlebar + footer).
              "transform" =: "scale(min(100vw / 846px, 100vh / 680px))"
            , "transform-origin" =: "center center"
            ]
        , CSS.selector_
            ".titlebar"
            [ "background-color" =: "#1a1a1a"
            , "border" =: "2px solid #555"
            , "color" =: "#ffd000"
            , CSS.fontSize "22px"
            , CSS.fontWeight "bold"
            , CSS.textAlign "center"
            , CSS.padding (CSS.px 6)
            , "letter-spacing" =: "4px"
            ]
        , CSS.selector_
            ".layout"
            [ CSS.display "flex"
            , "flex-direction" =: "row"
            , "gap" =: "12px"
            , "align-items" =: "stretch"
            ]
        , CSS.selector_
            ".pit"
            [ "background-color" =: "#000000"
            ]
        , CSS.selector_
            ".panel"
            [ CSS.display "flex"
            , "flex-direction" =: "column"
            , "gap" =: "12px"
            , "width" =: "90px"
            ]
        , CSS.selector_
            ".panel.wide"
            [ "width" =: "170px"
            ]
        , CSS.selector_
            ".stack"
            [ "flex" =: "1"
            , CSS.display "flex"
            , "flex-direction" =: "column"
            , "gap" =: "3px"
            , "border" =: "2px solid #00cc00"
            , CSS.padding (CSS.px 4)
            ]
        , CSS.selector_
            ".seg"
            [ "flex" =: "1"
            ]
        , CSS.selector_
            ".infobox .label"
            [ "color" =: "#00cc00"
            , CSS.fontSize "13px"
            , CSS.fontWeight "bold"
            , CSS.textAlign "center"
            , "margin-bottom" =: "2px"
            ]
        , CSS.selector_
            ".infobox .value"
            [ "border" =: "2px solid #2244cc"
            , "color" =: "#ffb000"
            , CSS.fontSize "18px"
            , CSS.fontWeight "bold"
            , CSS.textAlign "right"
            , CSS.padding (CSS.px 4)
            ]
        , CSS.selector_
            ".controls"
            [ "color" =: "#888888"
            , CSS.fontSize "14px"
            , CSS.textAlign "center"
            , "max-width" =: "846px"
            ]
        , -- menu screens
          CSS.selector_
            ".menuScreen"
            [ "border" =: "2px solid #2244cc"
            , "background-color" =: "#000000"
            , "width" =: "560px"
            , "min-height" =: "560px"
            , "box-sizing" =: "border-box"
            , "margin" =: "0 auto"
            , CSS.display "flex"
            , "flex-direction" =: "column"
            , "gap" =: "8px"
            , CSS.padding (CSS.px 24)
            ]
        , CSS.selector_
            ".menuScreen-title"
            [ "color" =: "#ffd000"
            , CSS.fontSize "28px"
            , CSS.fontWeight "bold"
            , CSS.textAlign "center"
            , "letter-spacing" =: "3px"
            , "margin-bottom" =: "12px"
            ]
        , CSS.selector_
            ".mrow"
            [ "color" =: "#00cc00"
            , CSS.fontSize "20px"
            , CSS.fontWeight "bold"
            , CSS.padding (CSS.px 8)
            , CSS.textAlign "center"
            , "cursor" =: "pointer"
            ]
        , CSS.selector_
            ".srow"
            [ "color" =: "#00cc00"
            , CSS.fontSize "17px"
            , CSS.fontWeight "bold"
            , CSS.padding (CSS.px 6)
            , CSS.display "flex"
            , "justify-content" =: "space-between"
            , "cursor" =: "pointer"
            ]
        , CSS.selector_
            ".sel"
            [ "background-color" =: "#1a1a1a"
            , "color" =: "#ffd000"
            ]
        , CSS.selector_
            ".note"
            [ "color" =: "#00cc00"
            , CSS.fontSize "15px"
            , CSS.textAlign "center"
            ]
        , CSS.selector_
            ".note.bright"
            [ "color" =: "#ffd000"
            , CSS.fontSize "18px"
            , CSS.fontWeight "bold"
            ]
        , CSS.selector_
            ".levels"
            [ CSS.display "flex"
            , "flex-direction" =: "column"
            , "align-items" =: "center"
            , "gap" =: "8px"
            , "margin-top" =: "16px"
            ]
        , CSS.selector_
            ".lvl"
            [ "color" =: "#006600"
            , CSS.fontSize "22px"
            , CSS.fontWeight "bold"
            , "cursor" =: "pointer"
            ]
        , CSS.selector_
            ".lvl.sel"
            [ "color" =: "#ffd000"
            ]
        , CSS.selector_
            ".name-entry"
            [ "border" =: "2px solid #2244cc"
            , "color" =: "#ffd000"
            , CSS.fontSize "26px"
            , CSS.fontWeight "bold"
            , CSS.textAlign "center"
            , CSS.padding (CSS.px 10)
            , "margin" =: "8px 60px"
            , "min-height" =: "34px"
            ]
        , CSS.selector_
            ".fame-table"
            [ "border" =: "2px solid #00cc00"
            , CSS.padding (CSS.px 8)
            , "margin-bottom" =: "10px"
            ]
        , CSS.selector_
            ".frow"
            [ CSS.display "flex"
            , "gap" =: "10px"
            , "color" =: "#00cc00"
            , CSS.fontSize "16px"
            , CSS.fontWeight "bold"
            , CSS.padding (CSS.px 2)
            ]
        , CSS.selector_
            ".frank"
            [ "width" =: "36px"
            , CSS.textAlign "right"
            ]
        , CSS.selector_
            ".fname"
            [ "flex" =: "1"
            ]
        , CSS.selector_
            ".fscore"
            [ "color" =: "#ffb000"
            ]
        ]
