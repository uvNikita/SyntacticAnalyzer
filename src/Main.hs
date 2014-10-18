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
import qualified Data.Text.IO as TIO
import           System.Environment (getArgs)

import           Parser (parser)
import           FSM (runFSM, prettyError)
import           Expression (render, optimize)


parseExpr :: Text -> Text
parseExpr input = either (prettyError input) (render . optimize) (runFSM parser input)


main :: IO ()
main = do
    [input] <- getArgs
    TIO.putStrLn . parseExpr . pack $ input
