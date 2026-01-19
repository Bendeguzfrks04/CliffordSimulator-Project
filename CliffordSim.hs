module CliffordSim where
import CnotCorrection
import Control.Monad (foldM)
-- import System.Posix (ProcessTimes(elapsedTime))
-- import Data.Text.Internal.Builder.RealFloat.Functions (roundTo)
-- import GHC.Float (roundFloat)


type Qubit = Int
data Pauli = I | X | Y | Z
  deriving (Eq, Show)

-- Egy- és kétqubites Paulik
data PauliOp
  = P1 Qubit Pauli
  | P2 Qubit Qubit Pauli Pauli
  deriving (Eq, Show)


--from previous docs
data CliffordSymbolic
  = H Qubit
  | S Qubit
  | SDag Qubit
  | CX Qubit Qubit
  | CZ Qubit Qubit
  deriving (Eq, Show)

-- Nem-Clifford kapu: RZ
data NonClifford
  = R PauliOp Double
  deriving (Eq, Show)

-- Kapu
data Gate
  = C CliffordSymbolic
  | N NonClifford
  deriving (Eq, Show)




-- A frame Pauli-konjugációként van ábrázolva:
type Frame = PauliOp -> PauliOp

identityFrame :: Frame
identityFrame = id

-- Clifford acts to Paulis
-- Hadamard
applyH :: Qubit -> PauliOp -> PauliOp
applyH q (P1 q' p)
  | q /= q' = P1 q' p
  | otherwise =
      case p of
        X -> P1 q Z
        Z -> P1 q X
        Y -> P1 q Y
        I -> P1 q I
applyH _ op = op

applyS1 :: Pauli -> Pauli
applyS1 X = Y
applyS1 Y = X   -- ignoring the -1 phase
applyS1 Z = Z
applyS1 I = I

applyS :: Qubit -> PauliOp -> PauliOp

-- Single-qubit Paulis
applyS q (P1 q' p)
    | q == q'   = P1 q (applyS1 p)
    | otherwise = P1 q' p

-- Two-qubit Paulis
applyS q (P2 q1 q2 p1 p2)
    | q == q1 && q == q2 =
        P2 q1 q2 (applyS1 p1) (applyS1 p2)
    | q == q1 =
        P2 q1 q2 (applyS1 p1) p2
    | q == q2 =
        P2 q1 q2 p1 (applyS1 p2)
    | otherwise =
        P2 q1 q2 p1 p2

conjugateCX :: Pauli -> Pauli -> (Pauli, Pauli)
conjugateCX p_c p_t = case (p_c, p_t) of
    (I, I) -> (I, I)
    (I, X) -> (I, X)
    (I, Y) -> (Z, Y)
    (I, Z) -> (Z, Z)
    (X, I) -> (X, X)
    (X, X) -> (X, I)
    (X, Y) -> (Y, Z)
    (X, Z) -> (Y, Y)
    (Y, I) -> (Y, X)
    (Y, X) -> (Y, I)
    (Y, Y) -> (X, Z)
    (Y, Z) -> (X, Y)
    (Z, I) -> (Z, I)
    (Z, X) -> (Z, X)
    (Z, Y) -> (I, Y)
    (Z, Z) -> (I, Z)

-- CNOT
applyCX :: Qubit -> Qubit -> PauliOp -> PauliOp
applyCX c t p =
  case p of
    P1 q pauli
      | q == c ->
          case pauli of
            X -> P2 c t X X
            Z -> P1 c Z
            Y -> P2 c t Y X
            I -> P1 q I
      | q == t ->
          case pauli of
            X -> P1 t X
            Z -> P2 c t Z Z
            Y -> P2 c t Z Y
            I -> P1 q I
      | otherwise -> p

    P2 q0 q1 p0 p1
      | q0 == c && q1 == t ->
          let (p0', p1') = conjugateCX p0 p1
          in P2 q0 q1 p0' p1'
    _ -> p


-- update current step with C or N
updateFrame :: CliffordSymbolic -> Frame -> Frame
updateFrame (H q) f  = \p -> applyH q (f p)   
updateFrame (S q) f  = \p -> applyS q (f p)   
updateFrame (CX c t) f = \p -> applyCX c t (f p)    
updateFrame _ f = f

-- Non-Clifford conj 13. equation
conjugate :: Frame -> NonClifford -> NonClifford
conjugate f (R p theta) = 
    let p' = f p
    in R p' theta

-- Simulator allapota
data SimState = SimState
  { numQbits :: Int
  , frame :: Frame
  , applied :: [NonClifford] 
  , cnotTransitions :: Int 
  , cnotTime :: Double
  }

--Examples
--1) HRZ(theta)H -> RX(theta)
examplEasy :: [Gate]
examplEasy =
  [ C (H 0)
  , N (R (P1 0 Z) 1.0)
  , C (H 0)
  ]

--2) CX (I . RZ) CX -> RZZ
exampleHard :: [Gate]
exampleHard =
  [ C (CX 0 1)
  , N (R (P1 1 Z) 1.0)
  , C (CX 0 1)
  ]

--3) CX RZZ CX → RZ
exampleHappy :: [Gate]
exampleHappy =
  [ C (CX 0 1)
  , N (R (P2 0 1 Z Z) 1.0)
  , C (CX 0 1)
  ]

--3qubit
exampleMulti :: [Gate]
exampleMulti =
  [ C (H 0)                     
  , C (CX 0 1)                  
  , N (R (P1 1 Z) 0.5)          
  , C (CX 0 2)                 
  , N (R (P1 2 Z) 1.0)          
  ]

example1q :: [Gate]
example1q = [ C (H 1) , N (R (P1 1 Z) 0.5) ]

exampleWithCX :: [Gate]
exampleWithCX =
  [ C (H 0)
  , C (H 1)
  , C (CX 0 1)
  , C (CX 0 1)
  , C (S 0)
  , C (S 1)
  , C (CX 0 1)
  , C (CX 0 1)
  ]


step :: Int -> SimState -> Gate -> IO SimState
step i st@(SimState nq f ops cnt tm) gate = do
 putStrLn ("--- Step " ++ show i ++ " ---")
 putStrLn ("Gate: " ++ show gate)

 case gate of

    -- case of Clifford gate
    C (CX c t) -> do
      putStrLn "Handling CNOT with correction"
      --putStrLn "Type: Clifford"
      putStrLn "Frame before:"
      printFrameExample f nq
      let cl2 = extractClifford2 f c t
      stats <- correctionUntilGood cl2
      putStrLn ("CNOT transitions: " ++ show (transitions stats))
      putStrLn ("CNOT correction time: " ++ show (runTime stats))
      putStrLn ("Extracted Clifford2: " ++ show cl2)
      let f' = updateFrame (CX c t) f
      putStrLn "Frame after:"
      printFrameExample f' nq
      putStrLn "Physical state unchanged\n"
      return (SimState nq f' ops (cnt + transitions stats) (tm  + runTime stats))
      


    -- Other Clifford gates
    C c -> do
      let f' = updateFrame c f
      return (SimState nq f' ops cnt tm)

    -- case of Non-Clifford gate
    N n -> do
      --putStrLn "Type: Non-Clifford"
      putStrLn ("Logical gate: " ++ show n)

      let nPhys = conjugate f n
      putStrLn ("Physical gate applied: " ++ show nPhys)
      putStrLn "Frame unchanged\n"

      return (SimState nq f (ops ++ [nPhys]) cnt tm)

step i st (C (CX c t)) = do
  putStrLn ("--- Step " ++ show i ++ " ---")
  putStrLn ("Gate: CX " ++ show c ++ " " ++ show t)
  handleCX c t st

classify :: PauliOp -> Clifford1
classify (P1 _ Z) = Id
classify (P1 _ X) = Hd
classify (P1 _ Y) = Hd
classify _        = Id


extractClifford2 :: Frame -> Qubit -> Qubit -> Clifford2Gate
extractClifford2 f c t =
  ( classify (f (P1 c Z))
  , classify (f (P1 t Z)) )

handleCX :: Qubit -> Qubit -> SimState -> IO SimState
handleCX c t st@(SimState nq f ops cnt tm) = do
  let cl2 = extractClifford2 f c t

  stats <- correctionUntilGood cl2

  putStrLn ("CNOT transitions: " ++ show (transitions stats))
  putStrLn ("Correction runtime " ++ show (runTime stats))

  let f' = updateFrame (CX c t) f

  return (SimState nq f' ops 
          (cnt + transitions stats) (tm + runTime stats))




-- qbitek allapotanak kiirasa az egyes lepesek utan
printFrameExample :: Frame -> Int -> IO ()
printFrameExample f nq = do
    mapM_ printOne [0 .. nq-1]
    where
        printOne q = do
            putStrLn ("Qbit " ++ show q ++ ":")
            putStrLn (" X on qubit " ++ show q ++ "->" ++ show (f (P1 q X)))
            putStrLn (" Y on qubit " ++ show q ++ "->" ++ show (f (P1 q Y)))
            putStrLn (" Z on qubit " ++ show q ++ "->" ++ show (f (P1 q Z)))

-- print out what would happen with the statevector
applyFinalFrame :: Frame -> Int -> IO ()
applyFinalFrame f nq = do
    putStrLn "\n=== Final frame correction ==="
    --putStrLn "Applying inverse on final Clifford Frame. Effect on Z Pauil:"
    mapM_ printQbit [0 .. nq-1]
    where
        printQbit q = putStrLn ("Z on qbit " ++ show q ++ " would be correctd form " ++ show (f (P1 q Z)))


-- Futatas adott peldaval
run :: Int -> [Gate] -> IO SimState
run nq gates = foldM (\st (i,g) -> step i st g)
                     (SimState nq identityFrame [] 0 0.0)
                     (zip [1..] gates) 



main :: IO ()
main = do
  ---putStrLn "Starting Clifford frame simulation"
  ---putStrLn "Initial state: 3 qubits |000>"

  finalState <- run 2 exampleHard

  putStrLn "\nPhysical non-Clifford gates applied:"
  mapM_ print (applied finalState)
  applyFinalFrame (frame finalState) (numQbits finalState)

  putStrLn ("Total CNOT transitions: " ++ show (cnotTransitions finalState))
  putStrLn ("Total CNOT correction time: " ++ show (cnotTime finalState) ++ "s")
  
