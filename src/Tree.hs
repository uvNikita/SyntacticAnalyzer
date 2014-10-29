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
      Tree (..)
    , renderAsText
    , renderAsDiagram
) where


import           Data.Tree.Pretty (drawVerticalTree)
import qualified Data.Tree as NT
import           Data.Text (Text, pack, unpack)
import qualified Data.Text as T

import           Data.Monoid ((<>))
import           Diagrams.Prelude (Diagram, (#), fc, white, text, (~~)
                                 , with, (&), (.~), pad, scale
                                 , circle, centerXY, R2)
import           Diagrams.TwoD.Layout.Tree (renderTree, symmLayout', slHSep, slVSep)
import           Diagrams.Core.Types (Backend, Renderable)
import           Diagrams.Path (Path)
import qualified Diagrams.TwoD.Text as DText


data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving (Show)


toNTree :: Tree a -> NT.Tree a
toNTree (Leaf a) = NT.Node a []
toNTree (Branch a left right) = NT.Node a [toNTree left, toNTree right]


renderAsText :: (Show a) => Tree a -> Text
renderAsText = pack . drawVerticalTree . fmap show . toNTree


-- renderExpr (Leaf o) = o
-- renderExpr (Branch o l r) = T.concat ["(", renderExpr l, o, renderExpr r, ")"]


renderAsDiagram :: (Renderable DText.Text b
                  , Renderable (Path R2) b
                  , Backend b R2)
                 => Tree Text -> Diagram b R2
renderAsDiagram = enchance . render . toNTree
    where enchance = scale 10 . pad 1.1 . centerXY
          render tree = renderTree renderNode renderEdge layout
              where layout = symmLayout' (with & slHSep .~ 4 & slVSep .~ 4) tree
                    renderNode = (<> circle 1 # fc white) . text . unpack
                    renderEdge = (~~)
