{-# LANGUAGE FlexibleContexts      #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE TypeOperators         #-}
{-# LANGUAGE ViewPatterns          #-}

module Main where


import           Data.Array.Accelerate          ((:.), Acc, All (..), Exp,
                                                 Matrix, Vector, Z (..))
import qualified Data.Array.Accelerate          as A
import           Data.Array.Accelerate.LLVM.PTX

growth :: Int -> Int
growth n = head . A.toList . run $ A.sum concatParts where
  genDat :: Acc (Vector Int)
  genDat = A.generate (A.constant $ Z A.:. n) f where
    f :: Exp (Z :. Int) -> Exp Int
    f (A.unlift -> (Z A.:. x)) = A.mod x 100
  genArr :: Acc (Matrix Int)
  genArr = A.replicate (A.lift $ Z A.:. All A.:. (3 :: Exp Int)) genDat
  part x = A.compute $ A.sum (A.take x genArr)
  parts :: [Acc (Vector Int)]
  parts  = part . A.constant <$> [1..n]
  concatParts :: Acc (Vector Int)
  -- concatParts = foldl1 (\a b -> A.compute $ a A.++ b) parts --faster compile, but way slower
  concatParts = foldl1 (A.++) parts


main :: IO ()
main = g 2 where
  g n = do
    print $ growth n
    g (n + 1)
