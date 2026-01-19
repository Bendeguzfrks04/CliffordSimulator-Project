module CNOTprop where

import CliffordSim
import System.Random
import Control.Monad (foldM)
import Distribution.Compat.Binary (Binary(put))
--------------------------------------------------------------------------------
-- Types and basic definitions
--------------------------------------------------------------------------------

-- | A symbolic single-qubit Clifford represented as a sequence of generators
--   (H and S). Global phase is ignored.
type Clifford1q = [CliffordSymbolic]

--------------------------------------------------------------------------------
-- Single-qubit Clifford generation
--------------------------------------------------------------------------------

-- | Coset representatives for the single-qubit Clifford group modulo powers of S.
--   Any Clifford can be written as S^k * r, where r is one of these.
cosetReps :: Qubit -> [Clifford1q]
cosetReps q =
  [ []                               -- I
  , [H q]
  , [H q, S q]
  , [H q, S q, S q]
  , [H q, S q, S q, S q]
  , [H q, S q, H q]
  ]

-- | Generate S^k for k = 0..3
sPower :: Qubit -> Int -> Clifford1q
sPower q k = replicate k (S q)

-- | Generate a random single-qubit Clifford gate sequence on qubit q.
--   Example output: [S q, H q, S q, S q]
randomClifford1Seq :: Qubit -> IO Clifford1q
randomClifford1Seq q = do
  k <- randomRIO (0, 3)
  j <- randomRIO (0, length (cosetReps q) - 1)
  pure (sPower q k ++ cosetReps q !! j)

-- | Lift a symbolic Clifford sequence into the Gate datatype.
randomClifford1Gates :: Qubit -> IO [Gate]
randomClifford1Gates q =
  map C <$> randomClifford1Seq q

-- | Generate random *local* Clifford gates on two qubits.
randomTwoLocalCliffords :: Qubit -> Qubit -> IO [Gate]
randomTwoLocalCliffords q0 q1 = do
  c0 <- randomClifford1Gates q0
  c1 <- randomClifford1Gates q1
  pure (c0 ++ c1)

--------------------------------------------------------------------------------
-- Frame update logic
--------------------------------------------------------------------------------

-- | Update a Clifford frame with a single gate.
updateFrameGate :: Gate -> Frame -> Frame
updateFrameGate (C g) f = updateFrame g f
updateFrameGate _     f = f

-- | Update a frame with a full circuit.
updateFrameCircuit :: [Gate] -> Frame -> Frame 
updateFrameCircuit gates f = foldl (flip updateFrameGate) f gates

--------------------------------------------------------------------------------
-- Pauli evolution utilities
--------------------------------------------------------------------------------

-- | Test Paulis used to verify normalization of CNOT.
--   These generate the full Pauli group under conjugation.
testPaulis :: [PauliOp]
testPaulis =
  [ P1 0 X
  , P1 0 Z
  , P1 1 X
  , P1 1 Z
  ]

-- | Apply S† via S³.
applySDag :: Qubit -> PauliOp -> PauliOp
applySDag q = applyS q . applyS q . applyS q

-- | Conjugate a Pauli by a gate.
--   This is the Heisenberg-picture action.
applyGate :: Gate -> PauliOp -> PauliOp
applyGate (C (H q))      = applyH q
applyGate (C (S q))      = applyS q
applyGate (C (SDag q))   = applySDag q
applyGate (C (CX c t))   = applyCX c t
applyGate _              = id

-- | Apply a sequence of gates (right-to-left) to a Pauli operator.
applyGateSequence :: [Gate] -> PauliOp -> PauliOp
applyGateSequence gs p =
  foldr applyGate p gs

--------------------------------------------------------------------------------
-- CNOT normalization test
--------------------------------------------------------------------------------

-- | Check whether a frame f normalizes CNOT under conjugation by gate sequence g.
--
--   Expression: g · f · CX · f⁻¹ · g⁻¹ = CX
--
--   Verified by checking Pauli conjugation consistency.
normalizesCNOT :: Frame -> Qubit -> Qubit -> [Gate] -> Bool
normalizesCNOT f q0 q1 g =
  all checkPauli testPaulis
  where
    checkPauli p =
      applyGateSequence g (f (applyCX q0 q1 p))
        == applyCX q0 q1 (f p)

--------------------------------------------------------------------------------
-- Random search for CNOT normalizers
--------------------------------------------------------------------------------

-- | Randomly search for a local Clifford that normalizes CNOT.
--
--   Arguments:
--     shots  – remaining attempts
--     round  – number of frame refreshes
--     f      – current Clifford frame
--     q0 q1  – qubits
findRandomNormalizer
  :: Int -> Int -> Frame -> Qubit -> Qubit
  -> IO (Frame, [Gate], Int, Int)

-- | Refresh the frame if no success after shots exceeded.
findRandomNormalizer 0 round f q0 q1 = do
  m  <- randomTwoLocalCliffords q0 q1
  let f' = updateFrameCircuit m f
  findRandomNormalizer 10000 (round + 1) f' q0 q1

-- | Attempt a random candidate.
findRandomNormalizer shots round f q0 q1 = do
  candidate <- randomTwoLocalCliffords q0 q1
  if normalizesCNOT f q0 q1 candidate
     then pure (updateFrameCircuit candidate f, candidate, shots, round)
     else findRandomNormalizer (shots - 1) round f q0 q1

-- | Handle the CNOT correction process and print results.
handleCXcorrection :: Frame -> Qubit -> Qubit -> IO Frame
handleCXcorrection f q0 q1 = do
    let maxShots = 10000
    let start = 1
    (f', r, shots, rounds) <- findRandomNormalizer maxShots start f q0 q1
    putStrLn "CNOT normalization"
    putStrLn ("Attempts: " ++ show (maxShots - shots))
    putStrLn ("Rounds: " ++ show rounds)
    putStrLn ("Gates: " ++ show r)
    putStrLn "-------------------------"
    return f'

--------------------------------------------------------------------------------
-- Example driver
--------------------------------------------------------------------------------
main2 :: IO ()
main2 = do
    let initGates =
            [ C (S 0)
            , C (H 1)
            ]
        frame0 = applyGateSequence initGates

    putStrLn "-------------------------"
    putStrLn "Initial frame:\n"
    printFrameExample frame0 2
    putStrLn "\n"
    putStrLn "-------------------------"

    f' <- handleCXcorrection frame0 0 1
    putStrLn "Resulting frame:\n"
    printFrameExample f' 2
    putStrLn "\n"
    putStrLn "-------------------------"
    let n =  R (P1 1 Z) 1.0
    putStrLn ("Logical gate: " ++ show n)
    let nPhys = conjugate f' n
    putStrLn ("Physical gate applied: " ++ show nPhys)
    putStrLn "Frame: unchanged\n"
    putStrLn "-------------------------"
    let g = [C (H 0), C (S 1),C (H 1) ]
    let f'' = updateFrameCircuit g f'
    putStrLn "After applying normalizing gates, frame is:\n"
    printFrameExample f'' 2
    putStrLn "\n"
    putStrLn "-------------------------"
    f''' <- handleCXcorrection f'' 0 1
    putStrLn "Resulting frame:\n"
    printFrameExample f''' 2
    putStrLn "\n"
    putStrLn "-------------------------"
    let n =  R (P1 0 X) 1.0
    putStrLn ("Logical gate: " ++ show n)
    let nPhys = conjugate f''' n
    putStrLn ("Physical gate applied: " ++ show nPhys)
    putStrLn "Frame: unchanged\n"
    