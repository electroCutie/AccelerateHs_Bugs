{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where


import           Data.Array.Accelerate          ((:.), Acc, Exp, Scalar, Vector,
                                                 Z (..))
import qualified Data.Array.Accelerate          as A
import           Data.Array.Accelerate.LLVM.PTX


segmentedFoldBug :: forall a. (A.ToFloating Int a, A.Floating a, A.Ord a) => Acc (Scalar a)
segmentedFoldBug  = A.maximum $ A.foldSeg (-) 0 genDat segmentsAcc where
    segments = seg 0 1 where
      seg s n
        | s > 45000000 = []
        | n > 10       = seg s 1
        | otherwise    = n : seg (s + n) (n + 1)
    segSum = sum segments
    segCt  = length segments
    segmentsAcc :: Acc (Vector Int)
    segmentsAcc = A.use $ A.fromList (Z A.:. segCt) segments
    genDat :: Acc (Vector a)
    genDat = A.map A.toFloating $ A.generate (A.constant $ Z A.:. segSum) f where
      f :: Exp (Z :. Int) -> Exp Int
      f (A.unlift -> (Z A.:. x)) = A.mod x 100


main :: IO ()
main = do
  let
    doublesOkay = segmentedFoldBug :: Acc (Scalar Double)
    floatsFail  = segmentedFoldBug :: Acc (Scalar Float)
  print $ run doublesOkay
  print $ run floatsFail
