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
    , render
    , cons
    , consTo
    , reverse
    , optimize
    , calcTime
    , append
) where


import           Prelude hiding(reverse)
import           Data.Text (Text, pack)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.List as L
import           Data.List (find)

import           Operation (Operation(..), toChar, inverse, time)


data RawExpr = Operator Operation
             | Operand Text
             | RawExpr [RawExpr] deriving(Show, Eq)


type SplitRule = RawExpr -> Operation -> RawExpr -> Bool


render :: RawExpr -> Text
render (Operator op) = pack [toChar op]
render (Operand name) = name
render (RawExpr es) = T.concat ["(", inner, ")"]
    where inner = T.concat . map render $ es


consTo :: Int -> RawExpr -> RawExpr -> RawExpr
consTo 0 expr (RawExpr es)       = RawExpr $ expr : es
consTo n expr (RawExpr (e : es)) = RawExpr $ new  : es
    where new = consTo (n - 1) expr e
consTo _ _ _ =  error "cannot cons to this expression"


cons :: RawExpr -> RawExpr -> RawExpr
cons = consTo 0


append :: RawExpr -> RawExpr -> RawExpr
append (RawExpr e1) (RawExpr e2) = RawExpr $ e1 ++ e2
append _ _ = error "can append only RawExprs"


reverse :: RawExpr -> RawExpr
reverse (RawExpr es) = RawExpr . map reverse . L.reverse $ es
reverse e = e


pairs' :: Bool -> Operation -> SplitRule -> RawExpr -> RawExpr
pairs' inv neg rule (RawExpr (e1 : sep@(Operator op) : e2 : rest))
    | rule e1 op e2 = RawExpr [pairs'' e1, sep', pairs'' e2] `cons` pairs'' (RawExpr rest)
        where sep' = if inv then Operator (inverse op) else sep
              pairs'' = pairs' False neg rule
pairs' _ neg rule (RawExpr (e : es)) =
    pairs'' e `cons` pairs'' (RawExpr es)
    where pairs'' = pairs' inv neg rule
          inv = case e of
                    Operator op -> op == neg
                    _ -> False
pairs' _ _ _ e = e

pairs :: Operation -> SplitRule -> RawExpr -> RawExpr
pairs = pairs' False


apply :: (RawExpr -> RawExpr) -> RawExpr -> RawExpr
apply f expr = fst $ fromJust $ find same (zip es (tail es))
    where es = iterate f expr
          same (prev, curr) = prev == curr

optimize :: RawExpr -> RawExpr
optimize = optimizeLow . optimizeHigh
    where optimizeLow  = apply (cleanupBrackets . splitLow . splitBasicLow)
          optimizeHigh = apply (cleanupBrackets . splitHigh . splitBasicHigh)

          pairsLow  = pairs Diff
          pairsHigh = pairs Div

          splitBasicLow = pairsLow rule
                where rule (Operand _) Sum  (Operand _) = True
                      rule (Operand _) Diff (Operand _) = True
                      rule _ _ _                        = False

          splitLow = pairsLow rule
                where rule _ Sum  _ = True
                      rule _ Diff _ = True
                      rule _ _ _    = False

          splitHigh = pairsHigh rule
                where rule _ Mul _ = True
                      rule _ Div _ = True
                      rule _ _ _   = False

          splitBasicHigh = pairsHigh rule
                where rule (Operand _) Mul (Operand _) = True
                      rule (Operand _) Div (Operand _) = True
                      rule _ _ _                       = False


cleanupBrackets :: RawExpr -> RawExpr
cleanupBrackets (RawExpr [e]) = cleanupBrackets e
cleanupBrackets (RawExpr es) = RawExpr (map cleanupBrackets es)
cleanupBrackets e = e


calcTime :: RawExpr -> Int
calcTime (Operand  _) = 0
calcTime (Operator o) = time o
calcTime (RawExpr es) = sum . map calcTime $ es


-- commutative :: RawExpr -> [RawExpr]
-- commutative (RawExpr es) = [RawExpr $ sortBy (compare `on` calcTime) es]
-- commutative e = [e]


-- cleanup :: RawExpr -> RawExpr
-- cleanup = cleanupBrackets . cleanupOnes
--
-- cleanupOnes :: RawExpr -> RawExpr
-- cleanupOnes (RawExpr (Operand o1 : Operator op : Operand o2 : rest)) =
--     case (o1, op, o2) of
--         ("0", Sum, _  ) -> cleanupOnes $ RawExpr (Operand o2 : rest)
--         (_  , Sum, "0") -> cleanupOnes $ RawExpr (Operand o1 : rest)
--         ("1", Mul, _  ) -> cleanupOnes $ RawExpr (Operand o2 : rest)
--         (_  , Mul, "1") -> cleanupOnes $ RawExpr (Operand o1 : rest)
--         _               -> part `append` cleanupOnes (RawExpr (Operand o2:rest))
--             where part = RawExpr [Operand o1, Operator op]
-- cleanupOnes e = e
