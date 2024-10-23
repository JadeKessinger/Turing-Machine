{-|
Module       : TM
Description  : Runs an example Turing Machine which results in "dotTM.txt"
Maintainer   : Jade Kessinger
-}

module TM where 

import TMAST 
import TMSimulate
import System.IO
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map


machine = Machine {
    config = Config Tape {left = ["0", "1", "0", "1", "1"], 
                           right = [], 
                           tapeHead = "_"} "0", 
    states = Map.fromList [
    ("0", Map.fromList [
        ("0", Transition {write = "0", move = Lt, goto = "Halt"}), 
        ("1", Transition {write = "1", move = Rt, goto = "Halt"}),
        ("_", Transition {write = "_", move = Lt, goto = "1"})]),
    ("1", Map.fromList [
        ("0", Transition {write = "1", move = Lt, goto = "1"}), 
        ("1", Transition {write = "0", move = Lt, goto = "1"}),
        ("_", Transition {write = "_", move = Rt, goto = "2"})]),
    ("2", Map.fromList [
        ("0", Transition {write = "0", move = Rt, goto = "2"}),
        ("1", Transition {write = "1", move = Rt, goto = "2"}),
        ("_", Transition {write = "_", move = Rt, goto = "Halt"})])]}

resultConfig = simulate machine 
resultString = stringFormat resultConfig
dotString = dotFormat machine 
makeDotFile = writeFile "dotTM.txt" dotString

