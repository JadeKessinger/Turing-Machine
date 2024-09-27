module TM where 

import TMAST 
-- import TMParser
import TMSimulate
import System.IO
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map


-- if you are implementing parsing from a file, you will 
-- likely need something like this that does file IO
-- if you are not parsing from a file, you can likely
-- get away with not needing do notation
-- example:
-- ghci> runTM "../turing-machine-examples/example1.txt"
-- runTM :: String -> IO()
-- runTM filename = do
--   -- get the contents of the file into a string fileContents
--   -- if you are not doing this part, you will need to 
--   -- hard-code some machines in your abstract machine syntax
--   fileContents      <- readFile filename

--   -- parse the string into a turing machine
--   let machine       = parseTM fileContents

--   -- run the machine
--   let resultConfig  = simulate machine 

--   -- nicely format the results
--   let resultString  = stringFormat resultConfig

--   -- print the final configuration
--   putStrLn resultString

--   -- convert the TM to dot forma for visualization
--   let dotString     = dotFormat machine 

--   -- print dotString to REPL. You can consider 
--   -- printing to file instead if you like
--   putStrLn dotString


-- if you are not implementing the parser you probably
-- do not need something like the above. you can do something
-- more like this:
machine = Machine {config = Config Tape {left = ["0", "1", "0", "1", "1"], right = [], tapeHead = "_"} "0", 
                         states = Map.fromList [("0", Map.fromList [("0", Transition {write = "0", move = Lt, goto = "Halt"}), 
                                                                    ("1", Transition {write = "1", move = Rt, goto = "Halt"}),
                                                                    ("_", Transition {write = "_", move = Lt, goto = "1"})]),
                                                ("1", Map.fromList [("0", Transition {write = "0", move = Lt, goto = "1"}), 
                                                                    ("1", Transition {write = "1", move = Lt, goto = "1"}),
                                                                    ("_", Transition {write = "_", move = Rt, goto = "Halt"})])]}
resultConfig = simulate machine 
resultString = stringFormat resultConfig
dotString     = dotFormat machine 
makeDotFile   = writeFile "dotTM.txt" dotString

