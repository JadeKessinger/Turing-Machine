module TMParser where

import TMAST 

import ParserCombinators

-- currently, this just returns a meaningless
-- TuringMachine value. If you are doing the parser
-- you will need to change this to turn a string from
-- a text file into a Machine

parseTM :: String -> Machine
parseTM str = Machine-- parse parseMachine str

-- parseMachine :: Parser TuringMachine
-- parseMachine = some state <+> tape <+-> whitespace

-- state :: Parser State
-- state = text "state:" <+> sym <+> transitionMap >>=: \(s, t)   

-- tape :: Parser Tape
-- tape = 

-- A TM consists of blocks separated by the word "state"
-- then there's always "read", "write", etc.
-- trailing or leading whitespace is ok
