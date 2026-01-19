# CliffordSimulator-Project

We have 3 Haskell files: CliffordSim.hs, CnotCorrection.hs, CNOTprop.hs.

- CliffordSim.hs is the main file. It imports CnotCorrection.hs. 
- CNOTprop.hs handles the CNOT correction due to the propagation of the clifford frame. It imports CliffordSim.hs

# PART 1
In the CliffordSim.hs file contains several examples that you can run by set in the main function like that: 
finalState <- run <NUM OF QUBITS> <YOUR EXAMPLE>

The CnotCorrection.hs contains the functions for the CNOT transition implementation. Here you need to install the random library via cabal or stack to your computer first. I did with stack:
1. stack ghci CnotCorrection.hs
2. stack install random
3. stack ghci

Now the code should run. (Error message about the random library didn't disappear tho)

Dependencies:
SYS.random library 

# PART 2
CNOT propagation correction Simulation 

1. load file CNOTprop.hs
2. call main2