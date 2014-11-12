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
    , commutative
) where


import           Prelude hiding(reverse)
import           Data.Text (Text)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.List as L
import           Data.List (find, sortBy, groupBy, permutations)
import           Data.Function (on)

import qualified Operation as O
import           Operation (Operation(..), toChar)


data RawExpr = RawExpr Operation Elem deriving (Show, Eq)
data Elem = Term Text | Brackets [RawExpr] deriving (Show, Eq)


render :: RawExpr -> Text
render = T.tail . render'

render' :: RawExpr -> Text
render' (RawExpr op (Term name))   = toChar op `T.cons` name
render' (RawExpr op (Brackets es)) = toChar op `T.cons` T.concat ["(", inner', ")"]
    where inner = T.concat . map render' $ es
          inner' = case T.head inner of
                       '+' -> T.tail inner
                       _   -> inner


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


unsign :: RawExpr -> RawExpr
unsign (RawExpr _ e) = RawExpr Sum e


pairsB :: Operation -> [RawExpr] -> [RawExpr]
pairsB by (e1@(RawExpr op1 _) : e2@(RawExpr op2 _) : rest)
    | op2 == by || op2 == O.inverse by = new : pairsB by rest
    | otherwise                        = pairs by e1 : pairsB by (e2 : rest)
    where new = if O.negFor by op1
                    then RawExpr op1 (Brackets [ pairs by (unsign e1)
                                               , pairs by (inverse e2) ])
                    else RawExpr op1 (Brackets [ pairs by (unsign e1)
                                               , pairs by e2])
pairsB by [e] = [pairs by e]
pairsB _ []   = []

pairs :: Operation -> RawExpr -> RawExpr
pairs by (RawExpr op (Brackets [e1, e2])) =
    RawExpr op (Brackets [ pairs by e1, pairs by e2 ])
pairs by (RawExpr op (Brackets es)) = RawExpr op new
    where new = case pairsB by es of
                    es'@[RawExpr Diff _] -> Brackets es'
                    [RawExpr _ e] -> e
                    es'  -> Brackets es'
pairs _ e = e


apply :: (RawExpr -> RawExpr) -> RawExpr -> RawExpr
apply f expr = fst . fromJust . find same $ zip es (tail es)
    where es = iterate f expr
          same (prev, curr) = prev == curr

optimize :: RawExpr -> RawExpr
optimize = apply (pairs Sum) . apply (pairs Mul)


splitTerms' :: [RawExpr] -> [RawExpr]
splitTerms' (e1@(RawExpr op1 _) : e2 : rest)
    | isTerm e2 = term : terms
    | otherwise = splitTerms e1 : splitTerms' (e2 : rest)
    where (restTerm, rest') = span isTerm rest
          term  = RawExpr op1 (Brackets $ [splitTerms (unsign e1), splitTerms e2] ++ restTerm)
          terms = splitTerms' rest'
          isTerm (RawExpr op _) = op == Mul || op == Div
splitTerms' [e] = [splitTerms e]
splitTerms' []  = []


splitTerms :: RawExpr -> RawExpr
splitTerms (RawExpr op (Brackets es)) = RawExpr op (Brackets (splitTerms' es))
splitTerms e = e


weight' :: [RawExpr] -> Int
weight' (e1@(RawExpr op _) : es) = O.time op + weight e1 + weight' es
weight' []  = 0


weight :: RawExpr -> Int
weight (RawExpr _ (Term _)) = 0
weight (RawExpr _ (Brackets [])) = 0
weight (RawExpr _ (Brackets (e:es))) = weight e + weight' es


commutative' :: [RawExpr] -> [[RawExpr]]
commutative' es = map concat variants
    where sorted = sortBy (compare `on` weight) (splitTerms' es)
          grouped = groupBy ((==) `on` weight) sorted
          perms = map permutations grouped
          variants = sequence perms


commutative :: RawExpr -> [RawExpr]
commutative e@(RawExpr _ (Term _)) = [e]
commutative (RawExpr op (Brackets es)) = map brackets . commutative' $ es
    where brackets = RawExpr op . Brackets
