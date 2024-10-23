{-|
Module       : TMAST
Description  : The Abstract Syntax Tree for a Turing Machine
Maintainer   : Jade Kessinger
-}

module TMAST where 

import Data.Map (Map)
import qualified Data.Map as Map

data Tape = Tape {left :: [Symbol],
                  right :: [Symbol],
                  tapeHead :: Symbol} deriving Show

type Symbol = String

data Direction = Lt | Rt
               deriving (Show, Eq, Ord)

type ReadSym = Symbol

data Transition = Transition {write :: Symbol, 
                              move :: Direction, 
                              goto :: StateName} deriving Show

type Transitions = Map ReadSym Transition

type StateName = String

type CurrentState = StateName

data Machine = Machine {config :: Config,
                        states :: Map StateName Transitions} 
                        deriving Show 

data Config = Config Tape CurrentState deriving Show 

stringFormat :: Config -> String
stringFormat config = show config

dotFormat :: Machine -> String 
dotFormat Machine {config = Config tape startState, states = states} = 
    "digraph D {" ++ "\n\n" ++ 
    "init -> " ++ startState ++ ";" ++ "\n\n" ++
    statesFormat (Map.toList states) ++
    "Halt -> Halt;" ++ "\n\n" ++
    "}"

statesFormat :: [(StateName, Transitions)] -> String
statesFormat [] = ""
statesFormat ((stateName, transitions):otherStates) = 
    (transitionFormat stateName (Map.toList transitions)) ++ "\n" ++ statesFormat otherStates

transitionFormat :: StateName -> [(ReadSym, Transition)] -> String
transitionFormat _ [] = ""
transitionFormat stateName ((readSym, Transition {write = write, move = move, goto = goto}):otherTransitions) =
    stateName ++ " -> " ++ goto ++ " " ++
    "[label = \" " ++ readSym ++ ":(" ++ write ++ "," ++ moveLetter ++ ")\"];\n" ++
    (transitionFormat stateName otherTransitions)
    where moveLetter = case move of 
            Lt -> "L"
            Rt -> "R"

