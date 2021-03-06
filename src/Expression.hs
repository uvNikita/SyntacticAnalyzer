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
    , openBrackets
    , cleanup
) where


import           Prelude hiding(reverse)
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.List as L
import           Data.List (sortBy, groupBy, permutations)
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


apply :: Eq c => (c -> c) -> c -> c
apply f = last . takeWhile2 (/=) . iterate f


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
splitTerms (RawExpr op (Brackets es)) = RawExpr op splited
    where splited = case splitTerms' es of
                        es'@[RawExpr Diff _] -> Brackets es'
                        [RawExpr _ e] -> e
                        es'  -> Brackets es'
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
          (zeros, rest) = span ((== 0) . weight) sorted -- do not need to permutate zero-weight terms
          grouped = groupBy ((==) `on` weight) rest
          perms = map permutations grouped
          variants = sequence ([zeros] : perms)


commutative :: RawExpr -> [RawExpr]
commutative e@(RawExpr _ (Term _)) = [e]
commutative (RawExpr op (Brackets es)) = map (cleanup . brackets) . commutative' $ es
    where brackets = RawExpr op . Brackets


mulOp :: Operation -> Operation -> Operation
mulOp Sum  Diff = Diff
mulOp Diff Sum  = Diff
mulOp Sum  Sum  = Sum
mulOp Diff Diff = Sum
mulOp _    _    = error "Try to multiply wrong operations"

opTerm :: Operation -> RawExpr -> RawExpr -> RawExpr
opTerm op (RawExpr op1 e1) (RawExpr op2 e2) =
    RawExpr (mulOp op1 op2) (Brackets [RawExpr Sum e1, RawExpr op e2])
-- opTerm op (RawExpr op1 e1) (RawExpr op2 (Brackets (RawExpr opb eb : es))) =
--     RawExpr (mulOp op1 op2) (Brackets (RawExpr opb e1 : RawExpr op (Brackets (eb : es))))

multiply :: RawExpr -> RawExpr -> RawExpr
multiply = opTerm Mul


divide :: RawExpr -> RawExpr -> RawExpr
divide = opTerm Div


openOneBracket' :: [RawExpr] -> [RawExpr]
openOneBracket' (RawExpr op1 e1 : RawExpr Mul (Brackets es) : rest) =
    RawExpr op1 (Brackets (open es)) : rest
    where open = map (multiply (RawExpr Sum e1)) . splitTerms'
openOneBracket' (RawExpr op1 (Brackets es) : RawExpr Mul e2 : rest) =
    openOneBracket' swapped
    where swapped = RawExpr op1 e2 : RawExpr Mul (Brackets es) : rest
openOneBracket' (RawExpr op1 (Brackets es) : RawExpr Div e2 : rest) =
    RawExpr op1 (Brackets (open es)) : rest
    where open = map (`divide` RawExpr Sum e2) . splitTerms'
openOneBracket' (old@(RawExpr op (Brackets es)) : rest) =
    if es' == es
        then old : openOneBracket' rest
        else new : rest
    where es' = openOneBracket' es
          new = RawExpr op (Brackets es')
openOneBracket' (e:es) = e : openOneBracket' es
openOneBracket' []     = []

takeWhile2 :: (a -> a -> Bool) -> [a] -> [a]
takeWhile2 p (x:ys@(y:_))
  | p x y     = x : takeWhile2 p ys
  | otherwise = [x]
takeWhile2 _ [x] = [x]
takeWhile2 _ [] = []


openBrackets :: RawExpr -> [RawExpr]
openBrackets (RawExpr op (Brackets es)) =
    tail . takeWhile2 (/=) . map (cleanup . brackets) . iterate openOneBracket' $ es
    where brackets = RawExpr op . Brackets
openBrackets e = [e]


cleanup :: RawExpr -> RawExpr
-- cleanup (RawExpr op (Brackets es)) = RawExpr op . Brackets . concatMap rmBrs $ es
--     where rmBrs (RawExpr Sum (Brackets es))
--               | all isSum es = es
--           rmBrs e = [e]
--           isSum (RawExpr Sum _) = True
--           isSum _ = False
cleanup (RawExpr Diff (Brackets [RawExpr Diff el])) = cleanup $ RawExpr Sum el
cleanup (RawExpr Diff (Brackets [RawExpr op1 el1, RawExpr Diff el2])) =
    cleanup $ RawExpr Sum (Brackets [e1', e2'])
    where e1' = RawExpr (O.inverse op1) el1
          e2' = RawExpr Sum el2
cleanup (RawExpr op (Brackets [e])) = RawExpr op new
    where new = case cleanup e of
                    RawExpr Sum e' -> e'
                    e' -> Brackets [e']
cleanup (RawExpr op (Brackets es)) = RawExpr op $ Brackets (map cleanup es)
cleanup e = e
