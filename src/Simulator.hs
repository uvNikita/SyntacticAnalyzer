{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-----------------------------------------------------------------------------
--
-- Module      :  Simulator
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

module Simulator (
      simulate
    , renderPs
) where

import           Operation (time)
import           Tree (BTree(..), Task(..), Const)

import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Set (Set)
import qualified Data.Set as S
import           Data.List ((\\))

import           Data.Text (Text, pack, intercalate)
import qualified Data.Text as T
import           Data.Function (on)

import           Control.Monad.RWS (RWS, tell, ask, get, put, modify, execRWS)
import           Control.Monad (unless)

newtype Processor = Processor Integer deriving (Show, Eq, Enum, Ord)


data Process = Process { timeLeft :: Int
                       , task :: Task }

instance Show Process where
    show (Process { timeLeft, task }) = "[" ++ show task ++ "] " ++ "(" ++ show timeLeft ++ ")"

instance Eq Process where
    (==) = (==) `on` task

instance Ord Process where
    compare = compare `on` task

toProcess :: Task -> Process
toProcess t = Process { timeLeft = time . op $ t, task = t }


type ProcessList = Map Processor Process


renderPs :: ProcessList -> Text
renderPs ps = intercalate "\n" . map renderPair . M.toList $ ps
    where renderPair (processor, process) = T.concat [ pack . show $ processor
                                                     , ": "
                                                     , pack . show $ process ]

data State = State { done :: Set Task
                   , ps   :: ProcessList }

initState :: State
initState = State { done = S.empty, ps = M.empty }


data Config = Config { processors :: [Processor]
                     , alg :: BTree Task Const
                     , tasks :: Set Task}

type Simulator = RWS Config [ProcessList] State


getTasks :: BTree Task Const -> Set Task
getTasks (Binary task left right) = S.insert task rest
    where rest = S.union lefts rights
          lefts  = getTasks left
          rights = getTasks right
getTasks (Unary task child) = S.insert task rest
    where rest = getTasks child
getTasks (Leaf _) = S.empty


findReady :: Set Task -> BTree Task Const -> Set Task
findReady done tree = case tree of
    Binary root left right ->
        if not (root `S.member` done) && isDone left && isDone right
            then S.insert root ready
            else ready
        where lefts  = findReady' left
              rights = findReady' right
              ready  = S.union lefts rights

    Unary root child       ->
        if not (root `S.member` done) && isDone child
            then S.insert root ready
            else ready
        where ready = findReady' child

    Leaf _                 -> S.empty
    where isDone (Binary task _ _) = task `S.member` done
          isDone (Unary  task _  ) = task `S.member` done
          isDone (Leaf _) = True

          findReady' = findReady done


simulate :: Integer -> BTree Task Const -> [ProcessList]
simulate pnum inputAlg = hist
    where (_, hist) = execRWS simulation conf initState
          conf = Config { alg = inputAlg, processors = prs, tasks = ts }
          prs = [Processor 1 .. Processor pnum]
          ts = getTasks inputAlg


simulation :: Simulator ()
simulation = do
    step
    State { ps } <- get
    tell [ps]
    ready <- isFinished
    unless ready simulation


isFinished :: Simulator Bool
isFinished = do
    Config { tasks, alg } <- ask
    State { done, ps } <- get
    let allTasks = getTasks alg
    return $ M.null ps && done == tasks


step :: Simulator ()
step = do
    calculate
    flushTasks
    assign


assign :: Simulator ()
assign = do
    Config { alg, processors }  <- ask
    s@(State { done, ps }) <- get
    let freePrs = processors \\ M.keys ps
    let currTasks = map task . M.elems $ ps
    let ready' = S.toList $ findReady done alg
    let ready = ready' \\ currTasks
    let assigns = M.fromList $ zip freePrs ready
    let nps = fmap toProcess assigns
    put s { ps = M.union ps nps }

tick :: Process -> Process
tick p@(Process { timeLeft }) = p { timeLeft = timeLeft - 1 }


isDone :: Process -> Bool
isDone (Process { timeLeft = 0 }) = True
isDone _                          = False


calculate :: Simulator ()
calculate = modify (\ s@(State { ps }) -> s { ps = fmap tick ps })


flushTasks :: Simulator ()
flushTasks = do
    s@(State { done, ps }) <- get
    let (d, nps) = M.partition isDone ps
    let d' = S.fromList . M.elems . fmap task $ d
    put s { done = S.union done d', ps = nps }
