# Models of Computation: Turing Machines in Haskell

Final project for CS 131: Programming Languages at Harvey Mudd College (Spring 2024). 

For this final project, I simulated a Turing Machine in Haskell. Given a configuration and states, this project will produce the resulting configuration after the Turing Machine runs and a visual representation of the Turing Machine in the dot language, which can be run at [Web Graph Viz](http://www.webgraphviz.com/).  

Watch a video explanation of this project here: 
https://drive.google.com/file/d/1AYhrSBJHNBJkZ83T8FBg6q7tzFStS4Vd/view?usp=sharing

## How To Run This Project

1. Clone this project
2. Download Haskell's [ghci](https://downloads.haskell.org/ghc/latest/docs/users_guide/ghci.html) and run `ghci`
3. Run `:load TM` to see an example Turing Machine in action!
4. Type `resultString` to see the end configuration of running the example machine in `TM.hs`
5. Run `makeDotFile` to generate a dot file. You can copy-paste this file to [Web Graph Viz](http://www.webgraphviz.com/) to visualize this example TM

If you want to see a different Turing Machine, change the `machine` in `TM.hs`. Define the `Config` with the current state and left, right, and head of the tape - make sure the left tape is a list to the left of the tape head read from right to left (not left to right!). Define the states and their corresponding read, write, move, and resulting state. 

An example output is stored in `example`. The example machine flips all 0s to 1s and 1s to 0s. A more human-readable version of this TM with an example trace can be found in `flipBitsTM.txt`. The resulting configuration from running `TM.hs` on this machine is stored in `resultString.txt`, and the dot file produced is in `dotTM.txt`. The visualization of the machine is stored as `dotTM.pdf`.

## Implementation Details

### A Machine
A machine is implemented as the current configuration and a map of states. The configuration is represented by the tape relative to the head of the TM and the current state. The states are represented by the state name as the key and the defined transitions as the values.

### The Infinite Tape
I implemented an infinite tape as a left, right, and head of the tape. The left tape is implemented as a list containing the values directly left of the tape from right to left. The tape is oriented from right to left so that the item next to the head of the tape is easily accessible as the head of the list. The right of the tape is similarly implemented as a list directly right of the tape from left to right. The head of the tape is a single Symbol. The variable is called `tapeHead` instead of `head` since `head` is a keyword in Haskell. 

I simulate the tape as infinite by treating any values to the left of the defined left tape or the right of the defined right tape as blank. 

## Further Work

To build on this project, I would implement a Parser to translate a more human-readable text file of a machine into the Machine representation from my Abstract Syntax Tree. 

## Contributors

This project was written solely by me. Initially, `TM.hs` was provided without a machine. `TMAST.hs` was provided with empty stubs for `data Machine`, `data Config`, `stringFormat`, and `dotFormat`. `TMSimulate` provided the `Data.Map` import and an empty `simulate` stub.
