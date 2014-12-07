{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Operation
-- Copyright   :
-- License     :  AllRightsReserved
--
-- Maintainer  :
-- Stability   :
-- Portability :
--
-- |
--
-----------------------------------------------------------------------------

module Operation (
      Operation (..)
    , fromChar
    , inverse
    , negFor
    , toChar
    , time
) where


data Operation = Mul | Sum | Div | Diff deriving (Eq)


instance Show Operation where
    show o = [toChar o]

inverse :: Operation -> Operation
inverse Mul  = Div
inverse Div  = Mul
inverse Sum  = Diff
inverse Diff = Sum


negFor :: Operation -> Operation -> Bool
negFor Mul Div = True
negFor Sum Diff = True
negFor _ _ = False


time :: Operation -> Int
time Sum  = 1
time Diff = 1
time Mul  = 2
time Div  = 4


fromChar :: Char -> Operation
fromChar '+' = Sum
fromChar '-' = Diff
fromChar '*' = Mul
fromChar '/' = Div
fromChar c   = error $ "Invalid Opration character: " ++ [c]


toChar :: Operation -> Char
toChar Mul  = '*'
toChar Div  = '/'
toChar Sum  = '+'
toChar Diff = '-'
