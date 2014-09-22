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
    , ctx
) where


import Control.Monad.State (State, runState, get, put)


data FSM st ct ctx =
    FSM {
          initState :: st,
          isFinish :: st -> Bool,
          step :: st -> ct -> Either String (st, Action ctx (Maybe String)),
          initCtx :: ctx
        }


data SysContext =
    SysContext {
                 str :: String,
                 idx :: Int,
                 err :: String
               }


initSysContext :: SysContext
initSysContext = SysContext { str = "", idx = 0, err = "" }


data Context uctx = Context SysContext uctx


type Action uctx a = State (Context uctx) a


class CharType a where
    fromChar :: Char -> a


char :: Char -> Action uctx (Maybe String)
char c = do
    Context sc@(SysContext {str}) uc <- get
    put $ Context sc {str = str ++ [c]} uc
    return Nothing

incr :: Action uctx ()
incr = do
    Context sc@(SysContext {idx}) uc <- get
    put $ Context sc {idx = idx + 1} uc


ctx :: Action uctx uctx
ctx = do
    Context _ uc <- get
    return uc


runFSM :: (CharType ct, Eq st) => FSM st ct ctx -> String -> Either String String
runFSM fsm input = case result of
                        Left err -> Left $ show idx ++ ": " ++ err
                        Right s -> Right s
    where initContext = Context initSysContext (initCtx fsm)
          (result, Context (SysContext {idx}) _) = runState (loop (initState fsm) input)
                                                            initContext
          loop state [] | isFinish fsm state = do
                              Context (SysContext {str}) _ <- get
                              return $ Right str
                        | otherwise = return $ Left "Unexpected end of input"
          loop state (c:cs) = do
              let ct = fromChar c
              let next = step fsm state ct
              case next of
                  Left err -> return $ Left err
                  Right (ns, action) -> do
                      incr
                      r <- action
                      case r of
                          Just err -> return $ Left err
                          _ -> loop ns cs
