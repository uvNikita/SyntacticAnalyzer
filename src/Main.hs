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

import           Data.Text (Text, pack, intercalate)
import qualified Data.Text.IO as TIO
import           Data.List.Split (chunksOf)

import           Parser (parser)
import           FSM (runFSM, prettyError)
import           Expression (optimize, commutative, openBrackets)
import qualified Tree
import           Tree (exprToTree)
import           Data.Tree (Tree)
import qualified Expression as E

import           Control.Monad.Trans (liftIO)
import           Diagrams.Backend.Cairo (Cairo)
import           Diagrams.Backend.Gtk (toGtkCoords, renderToGtk)
import           Diagrams.TwoD.Align (alignT, centerXY)
import           Diagrams.Prelude (Diagram, R2, hcat, vcat, text, scale, pad)
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
            let comms = commutative expr
            let brackets = concatMap openBrackets comms
            let orig  = expr
            let origTree  = exprToTree (optimize orig)
            let commTrees = map (exprToTree . optimize) comms
            let bracketsTrees = map (exprToTree . optimize) brackets
            showTextExprs orig comms brackets
            if nogui
                then showTextTrees origTree commTrees bracketsTrees
                else showGUITrees origTree commTrees bracketsTrees


showTextExprs :: E.RawExpr -> [E.RawExpr] -> [E.RawExpr] -> IO ()
showTextExprs orig comms brackets = do
    TIO.putStrLn "Original:"
    TIO.putStrLn (E.render orig)
    TIO.putStrLn "Commutative Law:"
    TIO.putStrLn . intercalate "\n" . map E.render $ comms
    TIO.putStrLn "Open Brackets:"
    TIO.putStrLn . intercalate "\n" . map E.render $ brackets


showTextTrees :: Show a => Tree a -> [Tree a] -> [Tree a] -> IO ()
showTextTrees orig comms brackets = do
    TIO.putStrLn "Original:"
    TIO.putStrLn (Tree.renderAsText orig)
    TIO.putStrLn "Commutative Law:"
    TIO.putStrLn . intercalate "\n" . map Tree.renderAsText $ comms
    TIO.putStrLn "Open Brackets:"
    TIO.putStrLn . intercalate "\n" . map Tree.renderAsText $ brackets


writeText = scale 12 . pad 2.1 . centerXY . text


rows n = vcat . map (hcat . map alignT) . chunksOf n


showGUITrees :: Tree Text -> [Tree Text] -> [Tree Text] -> IO ()
showGUITrees orig comms brackets = do
    let diagram = vcat [ writeText "Original"
                       , Tree.renderAsDiagram orig
                       , writeText "Commutative Law"
                       , rows 2 . map Tree.renderAsDiagram $ comms
                       , writeText "Open Brackets"
                       , rows 2 . map Tree.renderAsDiagram $ brackets ]

    _ <- initGUI
    window <- windowNew
    scroll <- scrolledWindowNew Nothing Nothing
    canvas <- drawingAreaNew
    _ <- canvas `on` sizeRequest $ return (Requisition 2048 2048)
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
