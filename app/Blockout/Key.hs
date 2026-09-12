{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE ViewPatterns #-}

{- | Names for the @keyCode@ values the game reacts to, so key handlers and
on-screen buttons can say 'KeyEnter' rather than 13. Everything here is
exported.
-}
module Blockout.Key where

pattern KeyBackspace, KeyEnter, KeyEsc, KeySpace :: Int
pattern KeyBackspace = 8
pattern KeyEnter = 13
pattern KeyEsc = 27
pattern KeySpace = 32

pattern KeyPgUp, KeyPgDn, KeyEnd, KeyHome :: Int
pattern KeyPgUp = 33
pattern KeyPgDn = 34
pattern KeyEnd = 35
pattern KeyHome = 36

pattern ArrowLeft, ArrowUp, ArrowRight, ArrowDown :: Int
pattern ArrowLeft = 37
pattern ArrowUp = 38
pattern ArrowRight = 39
pattern ArrowDown = 40

pattern KeyA, KeyC, KeyD, KeyE, KeyF, KeyH, KeyM, KeyP, KeyQ, KeyS, KeyW :: Int
pattern KeyA = 65
pattern KeyC = 67
pattern KeyD = 68
pattern KeyE = 69
pattern KeyF = 70
pattern KeyH = 72
pattern KeyM = 77
pattern KeyP = 80
pattern KeyQ = 81
pattern KeyS = 83
pattern KeyW = 87

-- | A key of the digit row, by its digit: @Digit 4@ is the key 4.
pattern Digit :: Int -> Int
pattern Digit n <- (digitRow -> Just n)
    where
        Digit n = 48 + n

-- | A numpad key with NumLock on, by its digit: @Numpad 4@ is the numpad 4.
pattern Numpad :: Int -> Int
pattern Numpad n <- (numpad -> Just n)
    where
        Numpad n = 96 + n

digitRow, numpad :: Int -> Maybe Int
digitRow code
    | code >= 48 && code <= 57 = Just (code - 48)
    | otherwise = Nothing
numpad code
    | code >= 96 && code <= 105 = Just (code - 96)
    | otherwise = Nothing

-- | The digit a key stands for, on either the digit row or the numpad.
digitValue :: Int -> Maybe Int
digitValue code = case code of
    Digit n -> Just n
    Numpad n -> Just n
    _ -> Nothing
