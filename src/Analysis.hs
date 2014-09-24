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
    parser
) where

import Data.Char (isSpace)

import           FSM (FSM(FSM), CharType, ErrorMsg, Action, char, pass, getCtx, modifyCtx)
import qualified FSM


data State = ExprS | NumberS | DecimalS | VarS | SpaceS |
             OpenBracketS | CloseBracketS deriving (Eq)


data CT = Letter | Number | Operator | Minus | Space |
          Dot | OpenBracket | CloseBracket | Other deriving (Show)


type Brackets = Int

checkBrackets :: Action Brackets (Maybe ErrorMsg)
checkBrackets = do
    brackets <- getCtx
    return $ if brackets == 0
                then Nothing
                else Just "mismatched brackets"

instance CharType CT where
    fromChar c | c `elem` ['a'..'z'] ++ ['A'..'Z'] = Letter
               | c `elem` ['1'..'9'] = Number
               | c `elem` "+/*" = Operator
               | isSpace c = Space
    fromChar '-' = Minus
    fromChar '.' = Dot
    fromChar '(' = OpenBracket
    fromChar ')' = CloseBracket
    fromChar _ = Other


parser :: FSM State CT Brackets
parser = FSM {
    FSM.initState = ExprS,
    FSM.isFinish = isFinish,
    FSM.step = step,
    FSM.initCtx = 0,
    FSM.postProcess = checkBrackets
}


operator :: Char -> Action Brackets (Maybe ErrorMsg)
operator o = char ' ' >> char o >> char ' '


bracket :: Char -> Action Brackets (Maybe ErrorMsg)
bracket '(' = char '(' >> modifyCtx (+1) >> return Nothing
bracket ')' = do
    _ <- char ')'
    modifyCtx (\ i -> i - 1)
    bn <- getCtx
    return $ if bn < 0 then Just "Unexpected bracket" else Nothing
bracket _ = error "Wrong usage of bracket function"


step :: State -> CT -> Either ErrorMsg (State, Char -> Action Brackets (Maybe ErrorMsg))
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
step CloseBracketS Space = Right (SpaceS, char)
step CloseBracketS Operator = Right (ExprS, operator)
step CloseBracketS Minus = Right (ExprS, operator)
step CloseBracketS _ = Left "parse error on input"

step VarS Letter = Right (VarS, char)
step VarS Number = Right (VarS, char)
step VarS Space = Right (SpaceS, char)
step VarS Operator = Right (ExprS, operator)
step VarS Minus = Right (ExprS, operator)
step VarS CloseBracket = Right (CloseBracketS, bracket)
step VarS _ = Left "Wrong variable symbol"

step NumberS Number = Right (NumberS, char)
step NumberS Dot = Right (DecimalS, char)
step NumberS Space = Right (SpaceS, char)
step NumberS Operator = Right (ExprS, operator)
step NumberS Minus = Right (ExprS, operator)
step NumberS CloseBracket = Right (CloseBracketS, bracket)
step NumberS _ = Left "integer constant error"

step SpaceS Space = Right (SpaceS, pass)
step SpaceS Operator = Right (ExprS, \c -> char c >> char ' ')
step SpaceS Minus = Right (ExprS, \ _ -> char '-' >> char ' ')
step SpaceS CloseBracket = Right (CloseBracketS, bracket)
step SpaceS _ = Left "parse error on input"

step DecimalS Number = Right (DecimalS, char)
step DecimalS Space = Right (SpaceS, char)
step DecimalS CloseBracket = Right (CloseBracketS, bracket)
step DecimalS Operator = Right (ExprS, operator)
step DecimalS Minus = Right (ExprS, operator)
step DecimalS _ = Left "float constant error"


isFinish :: State -> Bool
isFinish NumberS = True
isFinish VarS = True
isFinish DecimalS = True
isFinish CloseBracketS = True
isFinish _ = False
