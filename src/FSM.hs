{-# LANGUAGE NamedFieldPuns #-}
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
    , char
) where


import Control.Monad.State (State, evalState, get, put)


data FSM st ct ctx =
    FSM {
          initState :: st,
          isFinish :: st -> Bool,
          step :: st -> ct -> Either String (st, Action ctx),
          initCtx :: ctx
        }


data SysContext =
    SysContext {
                 str :: String,
                 idx :: Int,
                 err :: String
               }


initSysContext = SysContext { str = "", idx = 0, err = "" }


data Context uctx = Context SysContext uctx


type Action uctx = State (Context uctx) ()


class CharType a where
    fromChar :: Char -> a


char :: Char -> Action uctx
char c = do
    Context sc@(SysContext {str}) uc <- get
    put $ Context sc {str = str ++ [c]} uc

incr :: Action uctx
incr = do
    Context sc@(SysContext {idx}) uc <- get
    put $ Context sc {idx = idx + 1} uc


runFSM :: (CharType ct, Eq st) => FSM st ct ctx -> String -> Either String String
runFSM fsm input = evalState (loop (initState fsm) input) initContext
    where initContext = Context initSysContext (initCtx fsm)
          loop state [] | isFinish fsm state = do
                              Context (SysContext {str}) _ <- get
                              return $ Right str
                        | otherwise = return $ Left "Unexpected end of input"
          loop state (c:cs) = do
              let ct = fromChar c
              let next = step fsm state ct
              case next of
                  Left err -> return $ Left err
                  Right (ns, act) -> do
                      incr
                      act
                      loop ns cs

