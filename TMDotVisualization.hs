{-|
Module       : TMDotVisualization
Description  : Formats a Turing Machine into dot language (http://www.webgraphviz.com/) for visualization
Maintainer   : Jade Kessinger
-}

module TMDotVisualization where 

import TMAST

import Data.Map (Map)
import qualified Data.Map as Map


formatTMAsDotProgram :: Machine -> String 
formatTMAsDotProgram Machine {config = Config tape startState, states = states} = 
    "digraph D {" ++ "\n\n" ++ 
        "init -> " ++ startState ++ ";" ++ "\n\n" ++
        statesFormat (Map.toList states) ++
        "Halt -> Halt;" ++ "\n\n" ++
    "}"

formatStates :: [(StateName, Transitions)] -> String
formatStates [] = ""
formatStates ((stateName, transitions):otherStates) = 
    (formatTransitions stateName (Map.toList transitions)) ++ "\n" ++ 
    formatStates otherStates

formatTransitions :: StateName -> [(ReadSymbol, Transition)] -> String
formatTransitions _ [] = ""
formatTransitions stateName ((readSym, Transition {write = write, move = move, goto = goto}):otherTransitions) =
    stateName ++ " -> " ++ goto ++ " " ++
    "[label = \" " ++ readSym ++ ":(" ++ write ++ "," ++ moveLetter ++ ")\"];\n" ++
    (formatTransitions stateName otherTransitions)
    where moveLetter = case move of 
        Lt -> "L"
        Rt -> "R"