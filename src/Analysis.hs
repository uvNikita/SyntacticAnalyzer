-----------------------------------------------------------------------------
--
-- Module      :  Analysis
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

module Analysis (

) where

import FSM


data State = Init | AState deriving (Eq)


data CT = A | Other


data Context =
    Context {
              brackets :: Int
            }


initContext = Context { brackets = 0 }


instance CharType CT where
    fromChar 'a' = A
    fromChar 'A' = A
    fromChar _ = Other

testFSM = FSM {
    initState = Init,
    isFinish = testFinish,
    step = testStep,
    initCtx = initContext
}

testStep :: State -> CT -> Either String (State, Action Context)
testStep Init A = Right (AState, char 'a')
testStep Init Other = Left "not a"
testStep AState A = Right (AState, char 'a')
testStep AState Other = Left "not a"

testFinish Init = True
testFinish AState = True
