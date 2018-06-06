{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where


import           Data.Array.Accelerate          (Acc, Scalar, Vector, Z (..))
import qualified Data.Array.Accelerate          as A
import           Data.Array.Accelerate.LLVM.PTX

import           Control.Monad                  (when)

import           GHC.Float
import           System.Console.CmdArgs
import           Text.Printf

segFoldTest :: TestArgs -> IO ()
segFoldTest (TestArgs segStart segSumMax doFloats doDoubles doInts) = foldl1 (>>) (loop <$> [ segStart - x | x <- [0..segStart-1]]) where
  dat :: Num a => [a]
  dat = cycle $ map fromIntegral [0..99::Int]

  loop :: Int -> IO ()
  loop segMax = do
    let
      softInts = maximum $ softwareSegFold segments dat :: Int
      ds = segmentedFoldBug :: Acc (Scalar Double)
      fs = segmentedFoldBug :: Acc (Scalar Float)
      is = segmentedFoldBug :: Acc (Scalar Int)
      d = head . A.toList $ run ds
      f = head . A.toList $ run fs
      i = head . A.toList $ run is
    putStrLn $ printf "%d max seg size" segMax
    putStrLn $ printf "   % 8d (software)" softInts
    when doDoubles $
      putStrLn $ printf "%s% 8.0f (gpu doubles)" (if d /= int2Double softInts then "!!!" else "   ") d
    when doFloats $
      putStrLn $ printf "%s% 8.0f (gpu floats)" (if f /= int2Float softInts then "!!!" else "   ") f
    when doInts $
      putStrLn $ printf "%s% 8d (gpu ints)" (if i /= softInts then "!!!" else "   ") i
    putStrLn ""
    where
      segments :: [Int]
      segments = seg 0 1 where
        seg s n
          | s > segSumMax = []
          | n > segMax    = seg s 1
          | otherwise     = n : seg (s + n) (n + 1)
      segSum :: Int
      segSum = sum segments
      segCt :: Int
      segCt  = length segments

      softwareSegFold :: Num a => [Int] -> [a] -> [a]
      softwareSegFold [] _      = []
      softwareSegFold _ []      = error "ran off end of list"
      softwareSegFold (s:ss) xs = sum (take s xs) : softwareSegFold ss (drop s xs)

      segmentedFoldBug :: forall a. (Num a, A.Num a, A.Ord a) => Acc (Scalar a)
      segmentedFoldBug = A.maximum $ A.fold1Seg (+) genAcc segmentsAcc where
          segmentsAcc :: Acc (Vector Int)
          segmentsAcc = A.use $ A.fromList (Z A.:. segCt) segments
          genAcc :: Acc (Vector a)
          genAcc = A.use $ A.fromList (Z A.:. segSum) dat

data TestArgs = TestArgs {
    segstart, segmax      :: Int,
    floats, doubles, ints :: Bool
  } deriving (Show, Data, Typeable)

sampleArgs :: TestArgs
sampleArgs = TestArgs {
    segstart  = (300 :: Int)          &= help "Maximum size of segments to start with",
    segmax    = (60 * 1000000 :: Int) &= help "Number of elements to run the test on",
    floats    = True                  &= help "perform GPU tests on floats",
    doubles   = True                  &= help "perform GPU tests on doubles",
    ints      = True                  &= help "perform GPU tests on ints"
  }

main :: IO ()
main = do
  testArgs <- cmdArgs sampleArgs
  segFoldTest testArgs
