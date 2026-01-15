# CliffordSimulator-Project

WE have two Haskell file here: CliffordSim.hs and CnotCorrection.hs. The CliffordSim.hs is basically the main file and I import CnotCorrection.hs here. 
In the CliffordSim.hs file contains several examples what you can run by set in the main function like that: 
finalState <- run <NUM OF QUBITS> <YOUR EXAMPLE>

The CnotCorrection.hs contains the functions for the CNOT transition implementation. Here you need to install the random library via cabal or stack to your computer first. I did with stack:
1. stack ghci CnotCorrection.hs
2. Stack install random
3. stack ghci

Now the code should run.(Error message about the random library didn't disappear tho)

Dependencies:
SYS.random library 
