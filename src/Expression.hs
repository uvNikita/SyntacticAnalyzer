{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Expression
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

module Expression (
      RawExpr (..)
    , Elem (..)
    , render
    , cons
    , consTo
    , reverse
    , optimize
) where


import           Prelude hiding(reverse)
import           Data.Text (Text)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.List as L
import           Data.List (find)

import qualified Operation as O
import           Operation (Operation(..), toChar)


data RawExpr = RawExpr Operation Elem deriving (Show, Eq)
data Elem = Term Text | Brackets [RawExpr] deriving (Show, Eq)


render :: RawExpr -> Text
render = T.tail . render'

render' :: RawExpr -> Text
render' (RawExpr op (Term name))   = toChar op `T.cons` name
render' (RawExpr op (Brackets es)) = toChar op `T.cons` T.concat ["(", T.tail inner, ")"]
    where inner = T.concat . map render' $ es


consTo :: Int -> RawExpr -> RawExpr -> RawExpr
consTo 0 expr (RawExpr op (Brackets es))       = RawExpr op (Brackets $ expr : es)
consTo n expr (RawExpr op (Brackets (e : es))) = RawExpr op (Brackets $ new  : es)
    where new = consTo (n - 1) expr e
consTo _ _ _ =  error "cannot cons to this expression"


cons :: RawExpr -> RawExpr -> RawExpr
cons = consTo 0


reverse :: RawExpr -> RawExpr
reverse (RawExpr op (Brackets es)) = RawExpr op . Brackets . map reverse . L.reverse $ es
reverse e = e

inverse :: RawExpr -> RawExpr
inverse (RawExpr op e) = RawExpr (O.inverse op) e

pairsB :: Operation -> [RawExpr] -> [RawExpr]
pairsB on (e1@(RawExpr op1 _) : e2@(RawExpr op2 _) : rest)
    | op2 == on || op2 == O.inverse on = new : pairsB on rest
    | otherwise                        = pairs on e1 : pairsB on (e2 : rest)
    where new = if O.negFor on op1
                    then RawExpr op1 (Brackets [ pairs on (inverse e1)
                                               , pairs on (inverse e2) ])
                    else RawExpr op1 (Brackets [pairs on e1, pairs on e2])
pairsB on [e] = [pairs on e]
pairsB _ []   = []

pairs :: Operation -> RawExpr -> RawExpr
pairs on (RawExpr op (Brackets es)) = RawExpr op new
    where new = case pairsB on es of
                    [RawExpr _ e] -> e
                    es'  -> Brackets es'
pairs _ e = e

apply :: (RawExpr -> RawExpr) -> RawExpr -> RawExpr
apply f expr = fst . fromJust . find same $ zip es (tail es)
    where es = iterate f expr
          same (prev, curr) = prev == curr

optimize :: RawExpr -> RawExpr
optimize = apply (pairs Sum) . apply (pairs Mul)
