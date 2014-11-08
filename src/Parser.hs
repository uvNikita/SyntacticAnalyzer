{-# LANGUAGE OverloadedStrings #-}
-----------------------------------------------------------------------------
--
-- Module      :  Parser
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

module Parser (
    parser
) where

import           Data.Char (isSpace)
import           Data.Text (Text)
import qualified Data.Text as T
import           Control.Monad (when)

import           FSM (FSM(FSM), CharType, ErrorMsg, Action, pass, getCtx, putCtx, modifyCtx)
import qualified FSM
import           Expression (RawExpr)
import qualified Expression as E
import qualified Operation as O


data State = ExprS | NumberS | DecimalS | VarS | SpaceS |
             OpenBracketS | CloseBracketS deriving (Eq)


data CT = Letter | Number | Operator | Minus | Space |
          Dot | OpenBracket | CloseBracket | Other deriving (Show)


data Context =
    Context {
              brackets :: Int,
              operand :: Text,
              result :: RawExpr
            }


checkBrackets :: Action Context (Maybe ErrorMsg)
checkBrackets = do
    Context {brackets} <- getCtx
    return $ if brackets == 0
                then Nothing
                else Just "mismatched brackets"


instance CharType CT where
    fromChar c | c `elem` ['a'..'z'] ++ ['A'..'Z'] = Letter
               | c `elem` ['0'..'9'] = Number
               | c `elem` "+/*" = Operator
               | isSpace c = Space
    fromChar '-' = Minus
    fromChar '.' = Dot
    fromChar '(' = OpenBracket
    fromChar ')' = CloseBracket
    fromChar _ = Other


parser :: FSM State CT Context RawExpr
parser = FSM {
    FSM.initState = ExprS,
    FSM.isFinish = isFinish,
    FSM.step = step,
    FSM.initCtx = Context { brackets = 0, result = E.RawExpr [], operand = "" },
    FSM.getResult = getResult
}


getResult :: Action Context (Either ErrorMsg RawExpr)
getResult = do
    err <- checkBrackets
    flush
    Context {result} <- getCtx
    let result' = E.reverse result
    return $ maybe (Right result') Left err


flush :: Action Context ()
flush = do
    Context { operand } <- getCtx
    when (operand /= "") $ append (E.Operand operand)
    modifyCtx (\ ctx -> ctx { operand = "" })


append :: RawExpr -> Action Context ()
append expr = do
    ctx@(Context { brackets, result }) <- getCtx
    let newresult = E.consTo brackets expr result
    putCtx $ ctx { result = newresult }


operator :: Char -> Action Context (Maybe ErrorMsg)
operator o = flush >> append (E.Operator (O.fromChar o)) >> return Nothing

char :: Char -> Action Context (Maybe ErrorMsg)
char c = do
    modifyCtx (\ ctx@(Context { operand }) -> ctx { operand = T.snoc operand c })
    return Nothing


bracket :: Char -> Action Context (Maybe ErrorMsg)
bracket '(' = do
    append (E.RawExpr [])
    modifyCtx (\ ctx@(Context { brackets }) -> ctx { brackets = brackets + 1 })
    return Nothing

bracket ')' = do
    ctx@(Context {brackets}) <- getCtx
    if brackets <= 0
        then return $ Just "Unexpected bracket"
        else putCtx (ctx { brackets = brackets - 1 }) >> return Nothing
bracket _ = error "Wrong usage of bracket function"


step :: State -> CT -> Either ErrorMsg (State, Char -> Action Context (Maybe ErrorMsg))
step ExprS Number = Right (NumberS, char)
step ExprS OpenBracket = Right (OpenBracketS, bracket)
step ExprS Letter = Right (VarS, char)
step ExprS Space = Right (ExprS, pass)
step ExprS _ = Left "parse error on input"

step OpenBracketS Minus = Right (ExprS, char)
step OpenBracketS Number = Right (NumberS, char)
step OpenBracketS OpenBracket = Right (OpenBracketS, bracket)
step OpenBracketS Letter = Right (VarS, char)
step OpenBracketS Space = Right (ExprS, pass)
step OpenBracketS _ = Left "parse error on input"

step CloseBracketS CloseBracket = Right (CloseBracketS, bracket)
step CloseBracketS Space = Right (SpaceS, pass)
step CloseBracketS Operator = Right (ExprS, operator)
step CloseBracketS Minus = Right (ExprS, operator)
step CloseBracketS _ = Left "parse error on input"

step VarS Letter = Right (VarS, char)
step VarS Number = Right (VarS, char)
step VarS Space = Right (SpaceS, pass)
step VarS Operator = Right (ExprS, operator)
step VarS Minus = Right (ExprS, operator)
step VarS CloseBracket = Right (CloseBracketS, \ c -> flush >> bracket c)
step VarS _ = Left "Wrong variable symbol"

step NumberS Number = Right (NumberS, char)
step NumberS Dot = Right (DecimalS, char)
step NumberS Space = Right (SpaceS, pass)
step NumberS Operator = Right (ExprS, operator)
step NumberS Minus = Right (ExprS, operator)
step NumberS CloseBracket = Right (CloseBracketS, \ c -> flush >> bracket c)
step NumberS _ = Left "integer constant error"

step SpaceS Space = Right (SpaceS, pass)
step SpaceS Operator = Right (ExprS, operator)
step SpaceS Minus = Right (ExprS, operator)
step SpaceS CloseBracket = Right (CloseBracketS, \ c -> flush >> bracket c)
step SpaceS _ = Left "parse error on input"

step DecimalS Number = Right (DecimalS, char)
step DecimalS Space = Right (SpaceS, pass)
step DecimalS CloseBracket = Right (CloseBracketS, \ c -> flush >> bracket c)
step DecimalS Operator = Right (ExprS, operator)
step DecimalS Minus = Right (ExprS, operator)
step DecimalS _ = Left "float constant error"


isFinish :: State -> Bool
isFinish NumberS = True
isFinish VarS = True
isFinish DecimalS = True
isFinish CloseBracketS = True
isFinish _ = False
