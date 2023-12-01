module Day01 where

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

getInput :: IO [Int]
getInput = do
  input <- BSC.lines <$> BS.readFile "inputs/01.txt"
  return (read . BSC.unpack <$> input)

soln n [] = n
soln n [x] = n
soln n (x:(y:xs))
  | y > x = soln (n + 1) (y:xs)
  | otherwise = soln n (y:xs)


soln2 inputs = soln 0 windowSums
  where
    windowSums :: [Int]
    windowSums = (\(x, y, z) -> x + y + z) <$> zip3 (inputs) (tail inputs) (tail $ tail inputs)
