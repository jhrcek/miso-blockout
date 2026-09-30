{- | The practice-mode hint: where to put the falling piece, and the search
that finds it.
-}
module Blockout.Solver
    ( targetPlacement
    ) where

import Data.List (minimumBy, sort)
import qualified Data.Map.Strict as Map
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Sequence (ViewL (..), (|>))
import qualified Data.Sequence as Seq
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Word (Word64)

import Blockout.Mechanics
import Blockout.Types

{- | The cells the falling piece should land on: of all the places it can be
dropped into, the one that leaves the pit in the best shape (see 'cost'),
and among equally good ones the one reached with the fewest key presses
(moves and rotations) before the drop.
-}
targetPlacement :: Setup -> [Cell] -> [Cell] -> [Cell]
targetPlacement s wellCells start =
    snd (minimumBy (comparing fst) [((evaluate cs, presses), cs) | (cs, presses) <- landings])
  where
    w = Set.fromList wellCells
    -- Many positions drop onto the same cells (the piece only differs in
    -- how deep it hung before the drop), so score each landing once, with
    -- the fewest presses of any position leading to it. 'reachable' comes
    -- in order of presses, so the first one seen is that one.
    landings =
        Map.elems $
            Map.fromListWith
                (\_ first -> first)
                [ (positionKey s cs', (cs', presses))
                | (cs, presses) <- reachable s w start
                , let cs' = down (dropDistance s w cs) cs
                ]
    area = setupW s * setupL s
    wellFill = layerFill wellCells
    wellColumns = columnsOf wellCells
    -- Clearing only happens when the piece completes a layer, so the
    -- column profile of the pit can usually be extended rather than
    -- rebuilt from scratch.
    evaluate cs
        | null full = cost s (Map.size fill) (addColumns cs wellColumns)
        | otherwise =
            cost s (Map.size fill - length full) (columnsOf (fst (clearLayers s (cs ++ wellCells))))
      where
        fill = Map.unionWith (+) wellFill (layerFill cs)
        full = Map.filter (== area) fill

{- | How bad a pit is to be left with, lower being better: a weighted sum of
the things that make the next pieces hard to place.

  * holes: empty cells with a cube somewhere above them, which cannot be
    filled until everything above is cleared away
  * occupied layers: how close the stack is to the mouth of the pit
  * bumpiness: the height differences between neighbouring columns, as a
    ragged surface leaves few places a piece fits snugly
  * total height: the column heights summed, which favours placing pieces
    deep over stacking them up

The weights were tuned by letting the hint play thousands of games on the
predefined setups, to make it survive as long as possible. Holes weigh more
the wider the pit: in a small one the three dimensional pieces cannot help
leaving some, and shunning them at all costs only stacks the pit up faster.
-}
cost :: Setup -> Int -> Columns -> Double
cost s layers cols =
    holeWeight * fromIntegral holes
        + layerWeight * fromIntegral layers
        + bumpWeight * fromIntegral bump
        + heightWeight * fromIntegral (sum (Map.elems heights))
  where
    holeWeight = fromIntegral (setupW s * setupL s) / 3
    layerWeight = 0.5
    bumpWeight = 1
    heightWeight = 0.3
    heights =
        Map.fromList
            [ ((x, y), maybe 0 (\(top, _) -> setupD s - top) (Map.lookup (x, y) cols))
            | x <- [0 .. setupW s - 1]
            , y <- [0 .. setupL s - 1]
            ]
    holes = sum [setupD s - top - count | (top, count) <- Map.elems cols]
    height x y = Map.findWithDefault 0 (x, y) heights
    bump =
        sum [abs (height x y - height (x + 1) y) | x <- [0 .. setupW s - 2], y <- [0 .. setupL s - 1]]
            + sum [abs (height x y - height x (y + 1)) | x <- [0 .. setupW s - 1], y <- [0 .. setupL s - 2]]

-- | The number of cubes in each layer that has any.
layerFill :: [Cell] -> Map.Map Int Int
layerFill cs = Map.fromListWith (+) [(z, 1) | (_, _, z) <- cs]

{- | The profile of the pit seen from above: for each column (x, y) that
holds any cubes, the z of its topmost cube and how many cubes it holds.
-}
type Columns = Map.Map (Int, Int) (Int, Int)

columnsOf :: [Cell] -> Columns
columnsOf cs = addColumns cs Map.empty

addColumns :: [Cell] -> Columns -> Columns
addColumns cs cols = foldl' add cols cs
  where
    add m (x, y, z) = Map.insertWith merge (x, y) (z, 1) m
    merge (z, n) (top, count) = (min z top, n + count)

{- | Every position the piece can be brought into before it is dropped,
each with the fewest key presses that get it there, in order of that
number: a breadth-first search over single key presses.
-}
reachable :: Setup -> Set Cell -> [Cell] -> [([Cell], Int)]
reachable s w start = go (Set.singleton (key start)) (Seq.singleton (start, 0))
  where
    go seen queue = case Seq.viewl queue of
        EmptyL -> []
        (cs, n) :< rest ->
            let visit (seen', q) next
                    | Set.member k seen' = (seen', q)
                    | otherwise = (Set.insert k seen', q |> (next, n + 1))
                  where
                    k = key next
                (seen'', rest') = foldl' visit (seen, rest) (keyPresses s w cs)
             in (cs, n) : go seen'' rest'
    key = positionKey s

{- | A position of the piece packed into a single number, the same whatever
order its cells come in: each cell is a digit, in a base big enough to hold
the number of cells in the pit. A piece of up to five cubes in a pit of at
most 7x7x18 cells makes a number below 882^5 < 2^50, which needs a 64 bit
word even where 'Int' has only 32 bits, as in the browser.
-}
positionKey :: Setup -> [Cell] -> Word64
positionKey s = foldl' (\acc c -> acc * volume + index c) 0 . sort
  where
    volume = fromIntegral (pitVolume s)
    index (x, y, z) = fromIntegral ((z * setupL s + y) * setupW s + x)

{- | Where each key that moves or turns the piece takes it (see
'Blockout.Update.gameKey'): the four arrows, the four diagonals and the six
rotations. Keys that would not move the piece are left out.
-}
keyPresses :: Setup -> Set Cell -> [Cell] -> [[Cell]]
keyPresses s w cs =
    mapMaybe
        ($ cs)
        ( [ movePiece s w dx dy
          | (dx, dy) <- [(-1, 0), (1, 0), (0, -1), (0, 1), (-1, -1), (1, -1), (-1, 1), (1, 1)]
          ]
            ++ [ rotatePiece s w axis turn
               | axis <- [X, Y, Z]
               , turn <- [CW, CCW]
               ]
        )
