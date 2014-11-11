{-# LANGUAGE FlexibleContexts #-}
-----------------------------------------------------------------------------
--
-- Module      :  Tree
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

module Tree (
      exprToTree
    , renderAsText
    , renderAsDiagram
) where


import           Data.Tree.Pretty (drawVerticalTree)
import           Data.Tree (Tree(..))
import           Data.Text (Text, pack, unpack)

import           Data.Monoid ((<>))
import           Diagrams.Prelude (Diagram, (#), fc, white, text, (~~)
                                 , with, (&), (.~), pad, scale
                                 , circle, centerXY, R2)
import           Diagrams.TwoD.Layout.Tree (renderTree, symmLayout', slHSep, slVSep)
import           Diagrams.Core.Types (Backend, Renderable)
import           Diagrams.Path (Path)
import qualified Diagrams.TwoD.Text as DText

import           Expression (RawExpr(..), Elem(..))
import           Operation (toChar, Operation(..))


unaryMinusNode :: Elem -> Tree Text
unaryMinusNode e = Node minus [leaf]
  where minus = pack [toChar Diff]
        leaf = elemToTree e


elemToTree :: Elem -> Tree Text
elemToTree (Term name) = Node name []
elemToTree (Brackets [RawExpr Diff e1]) = unaryMinusNode e1
elemToTree (Brackets [RawExpr Diff e1, RawExpr op e2]) =
    Node root [left, right]
    where root  = pack [toChar op]
          left  = unaryMinusNode e1
          right = elemToTree e2
elemToTree (Brackets [RawExpr _ e1, RawExpr op e2]) =
    Node root [left, right]
    where root  = pack [toChar op]
          left  = elemToTree e1
          right = elemToTree e2
elemToTree _ = error "Non binary expression."


exprToTree :: RawExpr -> Tree Text
exprToTree (RawExpr _ e) = elemToTree e


renderAsText :: (Show a) => Tree a -> Text
renderAsText = pack . drawVerticalTree . fmap show


-- renderExpr (Leaf o) = o
-- renderExpr (Branch o l r) = T.concat ["(", renderExpr l, o, renderExpr r, ")"]


renderAsDiagram :: (Renderable DText.Text b
                  , Renderable (Path R2) b
                  , Backend b R2)
                 => Tree Text -> Diagram b R2
renderAsDiagram = enchance . render
    where enchance = scale 10 . pad 1.1 . centerXY
          render tree = renderTree renderNode renderEdge layout
              where layout = symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) tree
                    renderNode = (<> circle 1 # fc white) . text . unpack
                    renderEdge = (~~)
