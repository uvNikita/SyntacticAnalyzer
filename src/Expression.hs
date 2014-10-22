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
    , append
    , appendTo
    , reverse
    , optimize
    , cleanup
) where


import           Prelude hiding(reverse)
import           Data.Text (Text, pack)
import           Data.Maybe (fromJust)
import qualified Data.Text as T
import qualified Data.List as L
import           Data.List (find)

import           Operation (Operation(..), toChar, inverse)



data RawExpr = Operator Operation
             | Operand Text
             | RawExpr [RawExpr] deriving(Show, Eq)


type SplitRule = RawExpr -> Operation -> RawExpr -> Bool


render :: RawExpr -> Text
render (Operator op) = pack [toChar op]
render (Operand name) = name
render (RawExpr es) = T.concat ["(", inner, ")"]
    where inner = T.concat . map render $ es


appendTo :: Int -> RawExpr -> RawExpr -> RawExpr
appendTo 0 (RawExpr es)       expr = RawExpr $ expr : es
appendTo n (RawExpr (e : es)) expr = RawExpr $ new  : es
    where new = appendTo (n - 1) e expr
appendTo _ _ _ =  error "cannot append to this expression"


append :: RawExpr -> RawExpr -> RawExpr
append = appendTo 0


reverse :: RawExpr -> RawExpr
reverse (RawExpr es) = RawExpr . map reverse . L.reverse $ es
reverse e = e


pairs' :: Bool -> Operation -> SplitRule -> RawExpr -> RawExpr
pairs' inv neg rule (RawExpr (e1 : sep@(Operator op) : e2 : rest))
    | rule e1 op e2 = pairs'' (RawExpr rest) `append` RawExpr [pairs'' e1, sep', pairs'' e2]
        where sep' = if inv then Operator (inverse op) else sep
              pairs'' = pairs' False neg rule
pairs' _ neg rule (RawExpr (e : es)) =
    pairs'' (RawExpr es) `append` pairs'' e
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
    where optimizeLow  = apply (cleanup . splitLow . splitBasicLow)
          optimizeHigh = apply (cleanup . splitHigh . splitBasicHigh)

          pairsLow = pairs Diff
          pairsHigh = pairs Div

          splitBasicLow = pairsLow rule
                where rule (Operand _) Sum  (Operand _) = True
                      rule (Operand _) Diff (Operand _) = True
                      rule _ _ _ = False

          splitLow = pairsLow rule
                where rule _ Sum  _ = True
                      rule _ Diff _ = True
                      rule _ _ _ = False

          splitHigh = pairsHigh rule
                where rule _ Mul _ = True
                      rule _ Div _ = True
                      rule _ _ _ = False

          splitBasicHigh = pairsHigh rule
                where rule (Operand _) Mul (Operand _) = True
                      rule (Operand _) Div (Operand _) = True
                      rule _ _ _ = False


cleanup :: RawExpr -> RawExpr
cleanup (RawExpr [e]) = cleanup e
cleanup (RawExpr es) = RawExpr (map cleanup es)
cleanup e = e
