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
import           Expression (RawExpr(..), Elem(..))
import           Operation (toChar)


elemToTree :: Elem -> Tree Text
elemToTree (Term name) = Leaf name
elemToTree (Brackets [RawExpr _ e1, RawExpr op e2]) =
    Branch root left right
    where root  = pack [toChar op]
          left  = elemToTree e1
          right = elemToTree e2
elemToTree _ = error "Non binary expression."


exprToTree :: RawExpr -> Tree Text
exprToTree (RawExpr _ e) = elemToTree e
