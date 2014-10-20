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
    , render
) where


import           Data.Tree.Pretty (drawVerticalTree)
import qualified Data.Tree as NT
import           Data.Text (Text, pack)


data Tree a = Leaf a | Branch a (Tree a) (Tree a) deriving (Show)


toNTree :: Tree a -> NT.Tree a
toNTree (Leaf a) = NT.Node a []
toNTree (Branch a left right) = NT.Node a [toNTree left, toNTree right]


render :: (Show a) => Tree a -> Text
render = pack . drawVerticalTree . fmap show . toNTree
