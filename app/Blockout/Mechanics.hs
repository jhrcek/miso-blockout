{- | The pure mechanics of the falling piece: how it moves, turns, drops and
locks into the pit. Shared by the update logic, which plays them out on the
model, and "Blockout.Solver", which searches through them.
-}
module Blockout.Mechanics
    ( fits
    , movePiece
    , rotatePiece
    , down
    , dropDistance
    , clearLayers
    ) where

import Data.Set (Set)
import qualified Data.Set as Set

import Blockout.Types

-- | Do the cells all lie inside the pit, clear of the locked cubes?
fits :: Setup -> Set Cell -> [Cell] -> Bool
fits s w = all ok
  where
    ok c@(x, y, z) =
        inRange (0, setupW s - 1) x
            && inRange (0, setupL s - 1) y
            && inRange (0, setupD s - 1) z
            && Set.notMember c w

-- | Slide a piece sideways, if the cells it would move to are free.
movePiece :: Setup -> Set Cell -> Int -> Int -> [Cell] -> Maybe [Cell]
movePiece s w dx dy cs
    | fits s w moved = Just moved
    | otherwise = Nothing
  where
    moved = [(x + dx, y + dy, z) | (x, y, z) <- cs]

down :: Int -> [Cell] -> [Cell]
down k cs = [(x, y, z + k) | (x, y, z) <- cs]

-- | How far the piece can still fall.
dropDistance :: Setup -> Set Cell -> [Cell] -> Int
dropDistance s w cs = descend 0
  where
    descend k
        | fits s w (down (k + 1) cs) = descend (k + 1)
        | otherwise = k

{- | Remove the full layers from the pit, letting everything above them fall
into the gap. Returns the remaining cubes and the number of layers removed.
-}
clearLayers :: Setup -> [Cell] -> ([Cell], Int)
clearLayers s w0 = (w1, length full)
  where
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

-----------------------------------------------------------------------------
-- Rotation. Pieces rotate about the center of their bounding box, with a
-- few "kick" offsets tried so rotation works next to walls.
-----------------------------------------------------------------------------

{- | Turn a cell of a shape by 90 degrees within the shape's bounding box,
given the box's size along each axis. The result stays within a box of the
same size with its corner at the origin, just with two sides swapped.
-}
rotateCell :: Axis -> Turn -> (Int, Int, Int) -> Cell -> Cell
rotateCell axis turn (sx, sy, sz) (x, y, z) = case (axis, turn) of
    (X, CW) -> (x, sz - 1 - z, y)
    (X, CCW) -> (x, z, sy - 1 - y)
    (Y, CW) -> (sz - 1 - z, y, x)
    (Y, CCW) -> (z, y, sx - 1 - x)
    (Z, CW) -> (sy - 1 - y, x, z)
    (Z, CCW) -> (y, sx - 1 - x, z)

{- | Round @n@/2 to the nearest integer, breaking ties away from zero.
Recentering a rotated piece with this (rather than 'div', which floors)
keeps rotation a true cyclic action: the four offsets accumulated over a
full turn sum to zero, so repeating any rotation key returns the piece to
its starting cells instead of drifting sideways.
-}
roundHalf :: Int -> Int
roundHalf n = signum n * ((abs n + 1) `div` 2)

{- | Turn a piece by 90 degrees, if there is room for it.

The piece turns about the centre of its bounding box. If it does not fit
in place it is nudged back inside the pit: sideways off a wall, or
downward when a piece that grew taller would poke out through the mouth.
Upward kicks are deliberately excluded, as they would let repeated presses
of one rotation key climb the piece back up against gravity. Away from the
walls the in-place rotation always fits, so repeating any rotation key
cycles the piece through its orientations and back to its starting cells.
-}
rotatePiece :: Setup -> Set Cell -> Axis -> Turn -> [Cell] -> Maybe [Cell]
rotatePiece _ _ _ _ [] = Nothing
rotatePiece s w axis turn cs =
    case filter (fits s w) attempts of
        (good : _) -> Just good
        [] -> Nothing
  where
    ((mnx, mny, mnz), (mxx, mxy, mxz)) = bounds cs
    (sx, sy, sz) = (mxx - mnx + 1, mxy - mny + 1, mxz - mnz + 1)
    rel' = [rotateCell axis turn (sx, sy, sz) (x - mnx, y - mny, z - mnz) | (x, y, z) <- cs]
    (_, (mxx', mxy', mxz')) = bounds rel'
    (sx', sy', sz') = (mxx' + 1, mxy' + 1, mxz' + 1)
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
