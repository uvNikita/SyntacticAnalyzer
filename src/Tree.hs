{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
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


data Inode = Inode Int Text

instance Show Inode where
    show (Inode idx name) = show idx ++ ":" ++ unpack name


unaryMinusNode :: Int -> Elem -> Tree Inode
unaryMinusNode idx e = Node minus [leaf]
  where minus = Inode idx (pack [toChar Diff])
        leaf = elemToTree (idx * 2) e


elemToTree :: Int -> Elem -> Tree Inode
elemToTree idx (Term name) = Node (Inode idx name) []
elemToTree idx (Brackets [RawExpr Diff e1]) = unaryMinusNode idx e1
elemToTree idx (Brackets [RawExpr Diff e1, RawExpr op e2]) =
    Node root [left, right]
    where root  = Inode idx (pack [toChar op])
          left  = unaryMinusNode (2 * idx) e1
          right = elemToTree (2 * idx + 1) e2
elemToTree idx (Brackets [RawExpr _ e1, RawExpr op e2]) =
    Node root [left, right]
    where root  = Inode idx (pack [toChar op])
          left  = elemToTree (2 * idx)     e1
          right = elemToTree (2 * idx + 1) e2
elemToTree _ _ = error "Non binary expression."


exprToTree :: RawExpr -> Tree Inode
exprToTree (RawExpr _ e) = elemToTree 1 e


renderAsText :: (Show a) => Tree a -> Text
renderAsText = pack . drawVerticalTree . fmap show


-- renderExpr (Leaf o) = o
-- renderExpr (Branch o l r) = T.concat ["(", renderExpr l, o, renderExpr r, ")"]


renderAsDiagram :: (Renderable DText.Text b
                  , Renderable (Path R2) b
                  , Backend b R2
                  , Show a)
                 => Tree a -> Diagram b R2
renderAsDiagram = enchance . render
    where enchance = scale 10 . pad 1.1 . centerXY
          render tree = renderTree renderNode renderEdge layout
              where layout = symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) tree
                    renderNode = (<> circle 1.6 # fc white) . text . show
                    renderEdge = (~~)
