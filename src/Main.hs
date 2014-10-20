-----------------------------------------------------------------------------
--
-- Module      :  Main
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

module Main (
    main
) where

import           Data.Text (Text, pack)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import           System.Environment (getArgs)

import           Parser (parser)
import           FSM (runFSM, prettyError)
import           Expression (optimize)
import qualified Tree
import qualified Expression as E
import           Util (exprToTree)


parseExpr :: Text -> Text
parseExpr input = either (prettyError input) (render . optimize) (runFSM parser input)
    where render input = T.concat [tree, "\n", expr]
              where tree = Tree.render . exprToTree $ input
                    expr = E.render $ input


main :: IO ()
main = do
    [input] <- getArgs
    TIO.putStrLn . parseExpr . pack $ input
