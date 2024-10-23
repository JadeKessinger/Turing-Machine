{-|
Module       : TMSimulate
Description  : Simulates running a Turing Machine
Maintainer   : Jade Kessinger
-}

module TMSimulate where 

import TMAST 

import Data.Map (Map)
import qualified Data.Map as Map


simulate :: Machine -> Config
simulate Machine {config = Config tape "Halt", states = _} = 
    Config tape "Halt"
simulate machine = simulate (doStep machine)

doStep :: Machine -> Machine
doStep Machine {config = Config Tape {left = leftTape, right = rightTape, tapeHead = tapeHead} curState, states = states} = 
        case Map.lookup curState states of 
            Just transitions -> 
                case Map.lookup tapeHead transitions of 
                    Just Transition {write = write, move = move, goto = goto} ->
                        let newLeftTape     = stepLeftTape leftTape move write
                            newRightTape    = stepRightTape rightTape move write
                            newHead         = stepHead leftTape rightTape move in 
                        Machine {config = Config Tape {left = newLeftTape, right = newRightTape, tapeHead = newHead} goto, states = states}
                    Nothing -> error ("No transition defined for " ++ tapeHead)
            Nothing -> error ("No defined transitions for " ++ curState)

-- Moves the left tape one step to the left or right given a new write symbol
-- Makes the tape infinite by modeling empty lists as blank symbols
-- Returns thet updated left tape
stepLeftTape :: [Symbol] -> Direction -> Symbol -> [Symbol]
stepLeftTape [] Lt _            = ["_"]
stepLeftTape [] Rt write        = [write]
stepLeftTape leftTape Lt _      = tail leftTape
stepLeftTape leftTape Rt write  = write:leftTape

-- Moves the right tape to the left or right given a new write symbol
-- Makes the tape infinite by modeling empty lists as blank symbols
-- Returns thet updated right tape 
stepRightTape :: [Symbol] -> Direction -> Symbol -> [Symbol]
stepRightTape [] Lt write           = [write]
stepRightTape [] Rt _               = ["_"]
stepRightTape rightTape Lt write    = write:rightTape
stepRightTape rightTape Rt _        = tail rightTape

-- Moves the head of the tape to the left or right
-- Makes the tape infinite by modeling empty lists as blank symbols 
-- Returns thet updated head
stepHead :: [Symbol] -> [Symbol] -> Direction -> Symbol
stepHead [] [] _            = "_"
stepHead [] _ Lt            = "_"
stepHead _ [] Rt            = "_"
stepHead leftTape _ Lt      = head leftTape
stepHead _ rightTape Rt     = head rightTape
