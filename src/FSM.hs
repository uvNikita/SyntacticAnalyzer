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
    , getCtx
    , putCtx
    , modifyCtx
) where


import Control.Monad.State (State, runState, get, put)


type Error = String


data FSM st ct ctx =
    FSM {
          initState :: st,
          isFinish :: st -> Bool,
          step :: st -> ct -> Either String (st, Action ctx (Maybe Error)),
          initCtx :: ctx
        }


data SysContext =
    SysContext {
                 result :: String,
                 idx :: Int
               }


initSysContext :: SysContext
initSysContext = SysContext { result = "", idx = 0 }


data Context uctx = Context SysContext uctx


type Action uctx a = State (Context uctx) a


class CharType a where
    fromChar :: Char -> a


char :: Char -> Action uctx (Maybe Error)
char c = do
    Context sc@(SysContext {result}) uc <- get
    put $ Context sc {result = result ++ [c]} uc
    return Nothing


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


runFSM :: (CharType ct, Eq st) => FSM st ct ctx -> String -> Either Error String
runFSM fsm input = case result of
                        Left err -> Left $ show idx ++ ": " ++ err
                        Right r -> Right r
    where initContext = Context initSysContext (initCtx fsm)
          (result, Context (SysContext {idx}) _) = runState (loop (initState fsm) input)
                                                            initContext
          loop state [] | isFinish fsm state = do
                              Context (SysContext {result}) _ <- get
                              return $ Right result
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
