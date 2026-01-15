module CnotCorrection where

import System.Random
import Data.Time.Clock
import Distribution.Compat.Time (getCurTime)
import Data.Time (getCurrentTime, diffUTCTime)

-- TODO:
--from previous docs check the definition data
-- compare the explicit definition of the Clifford Gates

-- First, due to the S gate we need to define the imaginary unit
type C = (Double, Double) 
type M2 = ((C, C), (C, C))
i :: C
i = (0, 1)

add :: C -> C -> C
add (a, b) (c, d) = (a+c, b+d)

multi :: C -> C -> C
multi (a, b) (c, d) = (a*c - b*d, a*d + b*c)


data Clifford1 = Id 
    | Hd 
    | Sgate
    | SHd -- S compose H
    | HdS -- H compose S
  deriving (Eq, Show, Enum, Bounded)

-- Explicit Clifford gates
identity :: M2
identity = (((1,0), (0,0)),
     ((0,0), (1,0)))

hadamard :: M2
hadamard = let s = 1 / sqrt 2
      in (((s, 0), (s,0)),
          ((s,0), (-s, 0)))

phaseGate :: M2
phaseGate = (((1,0),(0,0)),
     ((0,0), i))


cliffordMap :: Clifford1 -> M2
cliffordMap Id = identity
cliffordMap Hd = hadamard
cliffordMap Sgate = phaseGate

data CorrectionStats = CorrectionStats
  { finalClifford :: Clifford2Gate
  , transitions   :: Int
  , runTime   :: Double   -- in sec
  }
  deriving Show


-- 2-qubit Clifford as tensor product of 2 single qubit (control, target)
type Clifford2Gate = (Clifford1, Clifford1)
propagateCNOT :: Clifford2Gate -> Either Clifford2Gate Clifford2Gate -- Left C_good, right C_bad
-- Cnot logic 
propagateCNOT (c1, c2)
   | c1 `elem` [Id, Sgate] = Left (c1, c2)
   | c2 `elem` [Id, Sgate] = Left (c1, c2)
   | otherwise = Right (c1, c2)

randomClifford1 :: IO Clifford1
randomClifford1 = do
    i <- randomRIO (fromEnum (minBound :: Clifford1),
                    fromEnum (maxBound :: Clifford1))
    return (toEnum i)


-- Correction loop
correctionUntilGood :: Clifford2Gate -> IO CorrectionStats
correctionUntilGood c0 = do
    start <- getCurrentTime
    (cFinal, n) <- go c0 0
    end <- getCurrentTime
    let dt = realToFrac (diffUTCTime end start)
    return (CorrectionStats cFinal n dt)
    where 
      go :: Clifford2Gate -> Int -> IO (Clifford2Gate, Int)
      go current n =  -- n here counts the number of transition
        case propagateCNOT current of
        Left good -> return (good, n)

         -- apply CNOT correction
        Right bad -> do
            corrected <- applyCNOTcorrection bad
            go corrected (n+1)


applyCNOTcorrection :: Clifford2Gate -> IO Clifford2Gate
applyCNOTcorrection (_, c2) = do 
    c1' <- randomClifford1
    return (c1', c2)

    --putStrLn ("Initial Clifford " ++ show c)
    --putStrLn ("Final Clifford " ++ show finnalCliff)
    --putStrLn("Runtime: " ++ show (diffUTCTime end start))
    --putStrLn ("CNOT_bad → CNOT_good transitions: " ++ show n)

exGood :: Clifford2Gate
exGood = (Id, Sgate)

exBad :: Clifford2Gate
exBad = (Hd, Id)

-- RUN FOREVER 
exWorst :: Clifford2Gate
exWorst = (HdS, SHd)


--main :: IO ()
--main = do
--  runCNOTcorrection exGood
--  runCNOTcorrection exBad
