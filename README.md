# General description
University project **dka-2-mka** for FLP subject.
Takes DFA and returns minimal DFA.
# Description of input and output formats
## Definition of Finit automata
Finit automata is a tuple **(Q, q, Sigma, F, P)** where:
* **Q** - set of states
* **q** - start state
* **Sigma** - alphabet
* **F** - set of finit states
* **P** - set of transitions.
## Input and output formats
Input and output formats are the same:
1. first line is list of states divided by comma
2. second line is start state
3. third line is list of finite states divided by commas
4. rest of lines are transitions in format **from,symbol,to** where **from** and **to** are states from the set **Q**, **symbol** is a symbol from alphabet **Sigma**.
# Description of minimization process.
1. Reads DFA from the file(stdin)
2. Validate DFA
3. Add SINK state and missing transitions(if is needed)
4. Create equivalence classes
5. Create new set of transitions between equivalence classes
6. Remove transitions with SINK state
7. Create new set of states(without SINK state)
8. Create new set of finite states
9. Create new start state
10. Construct new DFA
# Naming of new states
Each equivalence class is named based in its *the least state*.
*The least state* is the first state after alphanumeric ordering.
For example lets have a equivalence class made from states `["3", "2", "1"]`, *the least* state is `"1"`. New minimal DFA will have only state `"1"` in the new set of states **Q**.
As another example lets have a equivalence class `["st_A", "st_B", "st_C"]`. *The least class* represent's the given equivalence class  in minimal DFA will named `"st_A"`.
# Example
Lets have an input file with DFA: <br>
`1,2,3,4,5,6` <br>
`1` <br>
`1,6` <br>
`1,a,6` <br>
`1,b,2` <br>
`2,a,5` <br>
`2,b,4` <br>
`3,a,3` <br>
`3,b,6` <br>
`4,a,4` <br>
`4,b,1` <br>
`5,a,2` <br>
`5,b,3` <br>
`6,a,1` <br>
`6,b,5` <br>
The intermediate inner representation with equivalence classes will looks like: <br>
`["1", "6"],["2", "5"],["3", "4"]` <br>
`["1", "6"]` <br>
`["1", "6"]` <br>
`["1", "6"],a,["1", "6"]` <br>
`["1", "6"],b,["2", "5"]` <br>
`["2", "5"],a,["2", "5"]` <br>
`["2", "5"],b,["3", "4"]` <br>
`["3", "4"],a,["3", "4"]` <br>
`["3", "4"],b,["1", "6"]` <br>
The final result will looks like: <br>
`1,2,3` <br>
`1` <br>
`1` <br>
`1,a,1` <br>
`1,b,2` <br>
`2,a,2` <br>
`2,b,3` <br>
`3,a,3` <br>
`3,b,1` <br>

# Coplilation & launching process
## Compilation
It is possible to compile the source code with command `make`.
Another way is manual using `ghc` with command `ghc --make dka-2-mka.hs`
## Launching process:
The program is expected to launch as  `./dka-2-mka [-t] [-i] [input_source] `
* `-i` just read from input source, validate DFA and print it on stdout in the format described in the section about I/O formats
* `-t` read from input source, validate DFA and execute minimization process
* `input_source` defines an input source, in case of absence read from stdin
## Tests
As part of the project are tests which ran with command  `make tests`. Input automates  are contained in `./automates/inputs/` directory. The expected results are contained in `./automates/valid_outputs/` directory.
## Clean
Cleaning process remove outputs from tests and unnecessary files. Cleaning is ran with command `make clean`
