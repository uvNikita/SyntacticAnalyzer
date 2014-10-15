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
    , reverse
) where


import           Prelude hiding(reverse)
import           Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.List as L


data RawExpr = Operator Char
             | Operand Text
             | RawExpr [RawExpr] deriving(Show)


render :: RawExpr -> Text
render (Operator op) = pack [op]
render (Operand name) = name
render (RawExpr es) = T.concat ["(", inner, ")"]
    where inner = T.unwords . map render $ es


append :: RawExpr -> RawExpr -> Int -> RawExpr
append (RawExpr es)       expr 0 = RawExpr $ expr : es
append (RawExpr (e : es)) expr n = RawExpr $ new  : es
        where new = append e expr (n - 1)
append _ _ _ =  error "cannot append to this expression"


reverse :: RawExpr -> RawExpr
reverse (RawExpr es) = RawExpr . map reverse . L.reverse $ es
reverse e = e
