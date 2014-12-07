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
    , BTree (..)
    , Task (..)
    , Const (..)
) where


import           Data.Tree.Pretty (drawVerticalTree)
import qualified Data.Tree as T
import           Data.Tree (Tree)
import           Data.Text (Text, pack)

import           Data.Function (on)

import           Data.Monoid ((<>))
import           Diagrams.Prelude (Diagram, (#), fc, white, text, (~~)
                                 , with, (&), (.~), pad, scale
                                 , circle, centerXY, R2)
import           Diagrams.TwoD.Layout.Tree (renderTree, symmLayout', slHSep, slVSep)
import           Diagrams.Core.Types (Backend, Renderable)
import           Diagrams.Path (Path)
import qualified Diagrams.TwoD.Text as DText

import           Expression (RawExpr(..), Elem(..))
import           Operation (Operation(..))


data BTree a b = Binary a (BTree a b) (BTree a b)
               | Unary  a (BTree a b)
               | Leaf b


data Task = Task { idx :: Int, op :: Operation }

instance Eq Task where
    (==) = (==) `on` idx

instance Ord Task where
    compare = compare `on` idx

instance Show Task where
    show (Task idx op) = show idx ++ ":" ++ show op


newtype Const = Const Text

instance Show Const where
    show (Const name) = show name


toNTree :: (Show a, Show b) => BTree a b -> Tree String
toNTree (Leaf l) = T.Node (show l) []
toNTree (Binary root left right) =
    T.Node root' [left', right']
    where root'  = show root
          left'  = toNTree left
          right' = toNTree right
toNTree (Unary root child) = T.Node root' [child']
    where root'  = show root
          child' = toNTree child


unaryMinusNode :: Int -> Elem -> BTree Task Const
unaryMinusNode idx e = Unary minus leaf
  where minus = Task idx Diff
        leaf = elemToTree (idx * 2) e


elemToTree :: Int -> Elem -> BTree Task Const
elemToTree _ (Term name) = Leaf (Const name)
elemToTree idx (Brackets [RawExpr Diff e1]) = unaryMinusNode idx e1
elemToTree idx (Brackets [RawExpr Diff e1, RawExpr op e2]) =
    Binary root left right
    where root  = Task idx op
          left  = unaryMinusNode (2 * idx) e1
          right = elemToTree (2 * idx + 1) e2
elemToTree idx (Brackets [RawExpr _ e1, RawExpr op e2]) =
    Binary root left right
    where root  = Task idx op
          left  = elemToTree (2 * idx)     e1
          right = elemToTree (2 * idx + 1) e2
elemToTree _ _ = error "Non binary expression."


exprToTree :: RawExpr -> BTree Task Const
exprToTree (RawExpr _ e) = elemToTree 1 e


renderAsText :: (Show a, Show b) => BTree a b -> Text
renderAsText = pack . drawVerticalTree . toNTree


-- renderExpr (Leaf o) = o
-- renderExpr (Branch o l r) = T.concat ["(", renderExpr l, o, renderExpr r, ")"]


renderAsDiagram :: (Renderable DText.Text r
                  , Renderable (Path R2) r
                  , Backend r R2
                  , Show a
                  , Show b)
                 => BTree a b -> Diagram r R2
renderAsDiagram = enchance . render . toNTree
    where enchance = scale 10 . pad 1.1 . centerXY
          render tree = renderTree renderNode renderEdge layout
              where layout = symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) tree
                    renderNode = (<> circle 1.6 # fc white) . text
                    renderEdge = (~~)
