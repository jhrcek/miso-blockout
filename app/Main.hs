-----------------------------------------------------------------------------
{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

-----------------------------------------------------------------------------

{- | A clone of the classic 1989 DOS game BLOCKOUT (3D Tetris), with the
original's menu system: configurable pit dimensions and block sets,
predefined setups, practice mode and per-setup halls of fame.

See "Blockout.Types" for the model, "Blockout.Update" for the game and
menu logic and "Blockout.View" for the rendering.
-}
module Main where

-----------------------------------------------------------------------------
import Miso

import Blockout.Types (Action (..), Model, initialModel)
import Blockout.Update (gravitySub, keyDecoder, spinSub, updateModel)
import Blockout.View (sheet, viewModel)

-----------------------------------------------------------------------------
#ifdef WASM
#ifndef INTERACTIVE
foreign export javascript "hs_start" main :: IO ()
#endif
#endif

-----------------------------------------------------------------------------
main :: IO ()
#ifdef INTERACTIVE
main = reload defaultEvents app
#else
main = startApp defaultEvents app
#endif

-----------------------------------------------------------------------------
app :: App Model Action
app =
    (component initialModel updateModel viewModel)
        { styles = [Sheet sheet]
        , subs = [gravitySub, spinSub, windowSub "keydown" keyDecoder KeyDown]
        , mount = Just Boot
        }
