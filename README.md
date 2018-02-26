# General description
University project **dka-2-mka** to FLP subject.
Takes DFA as input and returns minimal DFA as output.
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
1. first line is a list of states divided by comma
2. second line is a start state
3. third line is a list of finite states divided by commas
4. rest of lines contains the transitions in format **from,symbol,to** where **from** and **to** are states from set the **Q**, **symbol** is a symbol from alphabet **Sigma** of DKA.
# Description of minimization process.
1. Reads DFA from the file(stdio)
2. Validate DFA //TODO
3. Add SINK state and missing transitions
4. Create equivalence classes
5. Create new set of transitions between equivalence classes(filter transitions with SINK state)
6. Create new set of states(without SINK state)
7. Create new set of finite states
8. Create new start state
9. Construct new DFA
# Naming of new states
Each equivalence class is named based in its *the least state*.
*The least state* is the first state after alphanumeric ordering.
For example lets have a class made from states `["3", "2", "1"]`, *the least* state is `"1"`. New minimal DFA will have only state `"1"` in the new set of states **Q**.
As another example lets have a equivalence class `["st_A", "st_B", "st_C"]`. *The least class* represent's an equivalence class  in minimal DFA will named `"st_A"`.
# Example
Lets have an input file with DFA:
`1,2,3,4,5,6`
`1`
`1,6`
`1,a,6`
`1,b,2`
`2,a,5`
`2,b,4`
`3,a,3`
`3,b,6`
`4,a,4`
`4,b,1`
`5,a,2`
`5,b,3`
`6,a,1`
`6,b,5`
The intermediate inner representation with equivalence classes will looks like:
`["1", "6"],["2", "5"],["3", "4"]`
`["1", "6"]`
`["1", "6"]`
`["1", "6"],a,["1", "6"]`
`["1", "6"],b,["2", "5"]`
`["2", "5"],a,["2", "5"]`
`["2", "5"],b,["3", "4"]`
`["3", "4"],a,["3", "4"]`
`["3", "4"],b,["1", "6"]`
The final result will looks like:
`1,2,3`
`1`
`1`
`1,a,1`
`1,b,2`
`2,a,2`
`2,b,3`
`3,a,3`
`3,b,1`

# Coplilation & launching process
//TODO
# Tests
//TODO