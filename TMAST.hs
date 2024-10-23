{-|
Module       : TMAST
Description  : The Abstract Syntax Tree for a Turing Machine
Maintainer   : Jade Kessinger
-}

module TMAST where 

import Data.Map (Map)
import qualified Data.Map as Map


type Symbol = String
type CurrentState = StateName
type StateName = String
type Transitions = Map ReadSymbol Transition
type ReadSymbol = Symbol

data Machine = Machine {config :: Config,
                        states :: Map StateName Transitions} deriving Show 

data Config = Config Tape CurrentState deriving Show 

data Tape = Tape {left :: [Symbol],
                  right :: [Symbol],
                  tapeHead :: Symbol} deriving Show

data Transition = Transition {write :: Symbol, 
                              move :: Direction, 
                              goto :: StateName} deriving Show

data Direction = Lt | Rt deriving (Show, Eq, Ord)

