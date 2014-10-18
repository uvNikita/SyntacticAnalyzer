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

import           Operation (Operation(..), toChar)



data RawExpr = Operator Operation
             | Operand Text
             | RawExpr [RawExpr] deriving(Show, Eq)


type SplitRule = (RawExpr, RawExpr, RawExpr) -> Bool


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


pairsPlus :: RawExpr -> RawExpr
pairsPlus = pairs rule
    where rule (Operand _, Operator Sum, Operand _) = True
          rule _ = False


pairsMul :: RawExpr -> RawExpr
pairsMul = pairs rule
    where rule (Operand _, Operator Mul, Operand _) = True
          rule _ = False


pairs :: SplitRule -> RawExpr -> RawExpr
pairs rule (RawExpr (e1:sep:e2:rest))
    | rule (e1, sep, e2) = pairs rule (RawExpr rest) `append` RawExpr [e1, sep, e2]
pairs rule (RawExpr (e:es)) = pairs rule (RawExpr es) `append` pairs rule e
pairs _ e = e


-- pdiv' :: RawExpr -> RawExpr
-- pdiv' rule (RawExpr (e1:sep:e2:rest))
--     | rule (e1, sep, e2) = pdiv' rule (RawExpr rest) `append` RawExpr [e1, sep, e2]
-- pdiv' rule (RawExpr (e:es)) = pdiv' rule (RawExpr es) `append` pdiv' rule e
-- pdiv' _ e = e


splitPlus :: RawExpr -> RawExpr
splitPlus = pairs rule
    where rule (_, Operator Sum, _) = True
          rule _ = False


splitMul :: RawExpr -> RawExpr
splitMul = pairs rule
    where rule (_, Operator Mul, _) = True
          rule _ = False


apply :: (RawExpr -> RawExpr) -> RawExpr -> RawExpr
apply f expr = fst $ fromJust $ find same (zip es (tail es))
    where es = iterate f expr
          same (prev, curr) = prev == curr

optimize :: RawExpr -> RawExpr
optimize = optimizePlus . optimizeMul
    where optimizeMul  = apply (cleanup . splitMul . pairsMul)
          optimizePlus = apply (cleanup . splitPlus . pairsPlus)

cleanup :: RawExpr -> RawExpr
cleanup (RawExpr [e]) = cleanup e
cleanup (RawExpr es) = RawExpr (map cleanup es)
cleanup e = e
