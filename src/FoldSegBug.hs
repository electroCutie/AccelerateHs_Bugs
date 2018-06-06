{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where


import           Data.Array.Accelerate          (Acc, Scalar, Vector, Z (..))
import qualified Data.Array.Accelerate          as A
import           Data.Array.Accelerate.LLVM.PTX

dat :: Num a => Int -> [a]
dat l = [ fromIntegral (x `mod` 100) | x <- [0..(l - 1)]]

segments :: [Int]
segments = seg 0 1 where
  seg s n
    | s > 45000000 = []
    | n > 10       = seg s 1
    | otherwise    = n : seg (s + n) (n + 1)
segSum :: Int
segSum = sum segments
segCt :: Int
segCt  = length segments

softwareSegFold :: Num a => [Int] -> [a] -> [a]
softwareSegFold [] _      = []
softwareSegFold _ []      = error "ran off end of list"
softwareSegFold (s:ss) xs = sum (take s xs) : softwareSegFold ss (drop s xs)

segmentedFoldBug :: forall a. (Num a, A.ToFloating Int a, A.Floating a, A.Ord a) => Acc (Scalar a)
segmentedFoldBug  = A.maximum $ A.fold1Seg (+) genAcc segmentsAcc where
    segmentsAcc :: Acc (Vector Int)
    segmentsAcc = A.use $ A.fromList (Z A.:. segCt) segments
    genAcc :: Acc (Vector a)
    genAcc = A.use $ A.fromList (Z A.:. segSum) (dat segSum)


main :: IO ()
main = do
  let
    softDoubles = maximum $ softwareSegFold segments (dat segSum)  :: Double
    doublesOkay = segmentedFoldBug :: Acc (Scalar Double)
    floatsFail  = segmentedFoldBug :: Acc (Scalar Float)
  print softDoubles -- expect 945
  print $ run doublesOkay
  print $ run floatsFail
