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
import           Expression (optimize)
import qualified Tree
import           Tree (Tree)
import qualified Expression as E
import           Util (exprToTree)

import Control.Monad.Trans (liftIO)
import Diagrams.Backend.Cairo (Cairo)
import Diagrams.Backend.Gtk (toGtkCoords, renderToGtk)
import Diagrams.Prelude (Diagram, R2)
import Graphics.UI.Gtk

main :: IO ()
main = do
    [in_] <- getArgs
    let input = pack in_
    case runFSM parser input of
        Left err -> TIO.putStrLn $ prettyError input err
        Right expr -> do
            let oexpr = optimize expr
            TIO.putStrLn $ E.render oexpr
            let tree = exprToTree oexpr
            showGUI tree
            showText tree


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
