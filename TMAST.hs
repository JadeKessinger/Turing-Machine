module TMAST where 

import Data.Map (Map)
import qualified Data.Map as Map

-- Representing the infinite tape
data Tape = Tape {left :: [Symbol],
                  right :: [Symbol],
                  tapeHead :: Symbol} deriving Show

-- Symbols/alphabet of the tape
type Symbol = String

-- Directions: left or right
data Direction = Lt | Rt
               deriving (Show, Eq, Ord)

-- Symbol seen on the tape
type ReadSym = Symbol

-- An instruction to read, write, move, and change states
data Transition = Transition {write :: Symbol, 
                              move :: Direction, 
                              goto :: StateName} deriving Show

-- Transitions for an associated state
type Transitions = Map ReadSym Transition

-- the Name of a state
type StateName = String

-- Current state of the Turing Machine
type CurrentState = StateName

-- a machine consists of the current config and 
-- a map of states to transitions
data Machine = Machine {config :: Config,
                        states :: Map StateName Transitions} 
                        deriving Show 

-- a configuration of the machine stores info about 
-- the state of the machine, the tape position, 
-- and the tape contents.
data Config = Config Tape CurrentState deriving Show 

-- prints out a nice string of a configuration
stringFormat :: Config -> String
stringFormat config = show config

-- formats the machine as a dotfile
dotFormat :: Machine -> String 
dotFormat Machine {config = Config tape startState, states = states} = 
    "digraph D {" ++ "\n\n" ++ 
    "init -> " ++ startState ++ ";" ++ "\n\n" ++
    statesFormat (Map.toList states) ++
    "Halt -> Halt;" ++ "\n\n" ++
    "}"

-- Format the states as strings for dot
statesFormat :: [(StateName, Transitions)] -> String
statesFormat [] = ""
statesFormat ((stateName, transitions):otherStates) = 
    (transitionFormat stateName (Map.toList transitions)) ++ "\n" ++ statesFormat otherStates

-- Format each transition as a string for dot
transitionFormat :: StateName -> [(ReadSym, Transition)] -> String
transitionFormat _ [] = ""
transitionFormat stateName ((readSym, Transition {write = write, move = move, goto = goto}):otherTransitions) =
    stateName ++ " -> " ++ goto ++ " " ++
    "[label = \" " ++ readSym ++ ":(" ++ write ++ "," ++ moveLetter ++ ")\"];\n" ++
    (transitionFormat stateName otherTransitions)
    where moveLetter = case move of 
            Lt -> "L"
            Rt -> "R"

