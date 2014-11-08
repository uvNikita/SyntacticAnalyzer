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

import           Parser (parser)
import           FSM (runFSM, prettyError)
import           Expression (optimize)
import qualified Tree
import           Tree (Tree)
import qualified Expression as E
import           Util (exprToTree)

import           Control.Monad.Trans (liftIO)
import           Diagrams.Backend.Cairo (Cairo)
import           Diagrams.Backend.Gtk (toGtkCoords, renderToGtk)
import           Diagrams.Prelude (Diagram, R2)
import           Graphics.UI.Gtk hiding (Settings)

import           System.Console.ArgParser ( parsedBy, andBy, ParserSpec, Descr(..)
                                          , mkApp, runApp, reqPos, boolFlag )


data Settings = Settings { input :: String, nogui :: Bool } deriving (Show)


argsParser :: ParserSpec Settings
argsParser = Settings
    `parsedBy` reqPos "expression" `Descr` "Expression to parse and analyze"
    `andBy` boolFlag "no-gui" `Descr` "Run program in text mode"


main :: IO()
main = do
    interface <- mkApp argsParser
    runApp interface analyze


analyze :: Settings -> IO ()
analyze (Settings {nogui, input}) = do
    let inputText = pack input
    case runFSM parser inputText of
        Left err -> TIO.putStrLn $ prettyError inputText err
        Right expr -> do
            let oexpr = optimize expr
            let tree = exprToTree oexpr
            TIO.putStrLn $ E.render oexpr
            if nogui
                then showText tree
                else showGUI tree


showText :: Show a => Tree a -> IO ()
showText tree = TIO.putStrLn $ Tree.renderAsText tree


showGUI :: Tree Text -> IO ()
showGUI tree = do
    let diagram = Tree.renderAsDiagram tree
    _ <- initGUI
    window <- windowNew
    scroll <- scrolledWindowNew Nothing Nothing
    canvas <- drawingAreaNew
    _ <- canvas `on` sizeRequest $ return (Requisition 512 512)
    scrolledWindowAddWithViewport scroll canvas
    set window [windowDefaultWidth := 256, windowDefaultHeight := 256
              , containerBorderWidth := 10
              , containerChild := scroll ]
    _ <- canvas `on` exposeEvent $ renderDiagram diagram
    _ <- onDestroy window mainQuit
    widgetShowAll window
    mainGUI


renderDiagram :: Diagram Cairo R2 -> EventM EExpose Bool
renderDiagram diagram = do
    win <- eventWindow
    liftIO . renderToGtk win . toGtkCoords $ diagram
    return True
