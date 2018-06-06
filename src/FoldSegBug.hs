{-# LANGUAGE DeriveDataTypeable    #-}
{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}

module Main where


import           Data.Array.Accelerate          (Acc, Scalar, Vector, Z (..))
import qualified Data.Array.Accelerate          as A
import           Data.Array.Accelerate.LLVM.PTX

import           Control.Monad                  (unless)

import           GHC.Float
import           System.Console.CmdArgs
import           Text.Printf

segFoldTest :: TestArgs -> IO ()
segFoldTest (TestArgs segStart segSumMax doFloats doDoubles doShorts doInts doLongs) = foldl1 (>>) (loop <$> [ segStart - x | x <- [0..segStart-1]]) where
  dat :: Num a => [a]
  dat = cycle $ map fromIntegral [0..99::Int]

  loop :: Int -> IO ()
  loop segMax = do
    let
      softInts = maximum $ softwareSegFold segments dat :: Int
      oneTest :: forall a b. (A.Num a, A.Ord a, Num a, Eq a, PrintfArg b) => Bool -> String -> (Int -> a) -> (a -> b) -> IO ()
      oneTest b fmt conv prnt = let
        v  = segmentedFoldBug :: Acc (Scalar a)
        v' = head . A.toList $ run v
        in unless b $
            putStrLn $ printf fmt (if v' /= conv softInts then "!!!" else "   ") (prnt v')

    putStrLn $ printf "%d max seg size" segMax
    putStrLn $ printf "   % 8d (software)" softInts

    oneTest doDoubles "%s% 8.0f (gpu doubles)" int2Double id
    oneTest doFloats  "%s% 8.0f (gpu floats)"  int2Float id

    oneTest doShorts "%s% 8d (gpu shorts)" (fromIntegral :: Int -> A.CShort) (fromIntegral :: A.CShort -> Int)
    oneTest doInts   "%s% 8d (gpu ints)"    id id
    oneTest doLongs  "%s% 8d (gpu longs)" (fromIntegral :: Int -> A.CLong) (fromIntegral :: A.CLong -> Integer)

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
    segstart, segmax                     :: Int,
    floats, doubles, shorts, ints, longs :: Bool
  } deriving (Show, Data, Typeable)

sampleArgs :: TestArgs
sampleArgs = TestArgs {
    segstart  = (260 :: Int)          &= help "Maximum size of segments to start with",
    segmax    = (60 * 1000000 :: Int) &= help "Number of elements to run the test on",
    floats    = False                 &= help "skip GPU tests on floats",
    doubles   = False                 &= help "skip GPU tests on doubles",
    shorts    = False                 &= help "skip GPU tests on shorts",
    ints      = False                 &= help "skip GPU tests on ints",
    longs     = False                 &= help "skip GPU tests on longs"
  }

main :: IO ()
main = do
  testArgs <- cmdArgs sampleArgs
  segFoldTest testArgs
