-----------------------------------------------------------------------------
--
-- Module      :  Util
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

module Util (
    exprToTree
) where


import           Data.Text (Text, pack)

import           Tree (Tree(..))
import           Expression (RawExpr(..))
import           Operation (toChar)


exprToTree :: RawExpr -> Tree Text
exprToTree (Operator op) = Leaf $ pack [toChar op]
exprToTree (Operand  op) = Leaf op
exprToTree (RawExpr [e1, Operator op, e2]) = Branch root left right
    where root = pack [toChar op]
          left = exprToTree e1
          right = exprToTree e2
exprToTree _ = error "Non binary expression."
