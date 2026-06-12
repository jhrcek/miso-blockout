{-# LANGUAGE OverloadedStrings #-}

{- | localStorage codecs for the setup (the original's @blockout.set@ file)
and the per-setup halls of fame.
-}
module Blockout.Persist
    ( setupKey
    , encodeSetup
    , decodeSetup
    , fameKey
    , encodeFame
    , decodeFame
    ) where

import Data.Char (toLower)
import Miso (MisoString, fromMisoString, ms)

import Blockout.Types

setupKey :: MisoString
setupKey = "blockout-setup"

encodeSetup :: Setup -> MisoString
encodeSetup (Setup w l d st sp) =
    ms (show (w, l, d, fromEnum st, fromEnum sp))

decodeSetup :: MisoString -> Maybe Setup
decodeSetup t = case reads (fromMisoString t) of
    [((w, l, d, st, sp), "")]
        | inside 3 7 w
        , inside 3 7 l
        , inside 6 18 d
        , inside 0 2 st
        , inside 0 2 sp ->
            Just (Setup w l d (toEnum st) (toEnum sp))
    _ -> Nothing
  where
    inside lo hi v = lo <= v && v <= (hi :: Int)

{- | Every pit/block-set combination has its own hall of fame; pits with
reversed width and length share one (manual p.13).
-}
fameKey :: Setup -> MisoString
fameKey s =
    ms $
        "blockout-hof-"
            <> show (min (setupW s) (setupL s))
            <> "x"
            <> show (max (setupW s) (setupL s))
            <> "x"
            <> show (setupD s)
            <> "-"
            <> map toLower (show (setupSet s))

encodeFame :: [(MisoString, Int)] -> MisoString
encodeFame entries =
    ms (show [(fromMisoString n :: String, sc) | (n, sc) <- entries])

decodeFame :: MisoString -> [(MisoString, Int)]
decodeFame t = case reads (fromMisoString t) of
    [(entries, "")] -> [(ms (n :: String), sc) | (n, sc) <- entries]
    _ -> []
