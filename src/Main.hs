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

import Data.Text (Text, pack)
import Data.Text.IO as TIO
import System.Environment (getArgs)

import Analysis (parser)
import FSM (runFSM, prettyError)


parseExpr :: Text -> Text
parseExpr input = either (prettyError input) (id) (runFSM parser input)


main :: IO ()
main = do
    [input] <- getArgs
    TIO.putStrLn . parseExpr . pack $ input
