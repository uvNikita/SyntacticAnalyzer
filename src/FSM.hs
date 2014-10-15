{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  FSM
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

module FSM (
      FSM (..)
    , runFSM
    , CharType(..)
    , Action
    , ErrorMsg
    , pass
    , getCtx
    , putCtx
    , modifyCtx
    , prettyError
) where


import           Control.Monad.State (State, runState, get, put)
import           Data.Text (Text, pack)
import qualified Data.Text as T


type ErrorMsg = Text


data ParseError = ParseError Int ErrorMsg deriving (Show)


prettyError :: Text -> ParseError -> Text
prettyError input (ParseError idx msg) =
    T.concat [input, "\n", marker, "\n", arrow, "\n", pack . show $ idx, ": ", msg]
    where marker = T.justifyRight (idx + 1) ' ' "ʌ"
          arrow  = T.justifyRight (idx + 1) '─' "┘"


data FSM st ct ctx res =
    FSM {
          initState :: st,
          isFinish :: st -> Bool,
          step :: st -> ct -> Either ErrorMsg (st, Char -> Action ctx (Maybe ErrorMsg)),
          initCtx :: ctx,
          getResult :: Action ctx (Either ErrorMsg res)
        }


data SysContext =
    SysContext {
                 idx :: Int
               }


initSysContext :: SysContext
initSysContext = SysContext { idx = 0 }


data Context uctx = Context SysContext uctx


type Action uctx a = State (Context uctx) a


class CharType a where
    fromChar :: Char -> a


pass :: Char -> Action uctx (Maybe ErrorMsg)
pass _ = return Nothing


incr :: Action uctx ()
incr = do
    Context sc@(SysContext {idx}) uc <- get
    put $ Context sc {idx = idx + 1} uc


getCtx :: Action uctx uctx
getCtx = do
    Context _ uc <- get
    return uc


putCtx :: uctx -> Action uctx ()
putCtx uc = do
    Context sc _ <- get
    put $ Context sc uc


modifyCtx :: (uctx -> uctx) -> Action uctx ()
modifyCtx f = do
    Context sc uc <- get
    let uc' = f uc
    put $ Context sc uc'


runFSM :: (CharType ct, Eq st) => FSM st ct ctx res -> Text -> Either ParseError res
runFSM fsm input = case runResult of
                        Left err -> Left $ ParseError idx err
                        Right r -> Right r
    where initContext = Context initSysContext (initCtx fsm)
          (runResult, Context (SysContext {idx}) _) = runState (loop (initState fsm) input)
                                                            initContext
          loop state str = case T.uncons str of
              Nothing -> finish state
              Just (c, cs) ->  do
                  r <- step' state c
                  case r of
                      Right ns -> loop ns cs
                      Left err -> return $ Left err
          finish state | isFinish fsm state = getResult fsm
                       | otherwise = return $ Left "Unexpected end of input"
          step' state c = do
              let ct = fromChar c
              let next = step fsm state ct
              case next of
                  Left err -> return $ Left err
                  Right (ns, action) -> do
                      r <- action c
                      case r of
                          Just err -> return $ Left err
                          Nothing -> incr >> return (Right ns)
