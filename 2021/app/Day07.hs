{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day07 where

import Debug.Trace
import Text.Megaparsec hiding (getInput)
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Semigroup
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Monad.IO.Class
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import Data.Foldable
import Data.String
import Data.Maybe
import Data.List
import Data.Bits
import Data.Array hiding (bounds)


parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s [Int]
parser_input = do
    sepBy1 num (symbol ",")
  where
    space = L.space space1 mzero mzero
    lexeme = L.lexeme space
    symbol = L.symbol space
    num = lexeme L.decimal

getInput :: IO [Int]
getInput = do
  txt <- T.readFile "inputs/07.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"

fuelToTarget :: [Int] -> Int -> Int
fuelToTarget crabs target = sum (fuelUsed <$> crabs)
  where
    fuelUsed crab = abs (crab - target)

soln = do
  input <- getInput
  return $ minimum $ fuelToTarget input <$> [0..(maximum input)]

fuelToTarget2 :: [Int] -> Int -> Int
fuelToTarget2 crabs target = sum (fuelUsed <$> crabs)
  where
    fuelUsed crab = sqrIsh (abs $ crab - target)
    sqrIsh x = (x * (x + 1)) `div` 2

candidates xs = [mean, mean + 1]
  where
    mean = floor (fromIntegral (sum xs) / fromIntegral n)
    n = length xs

soln2 = do
  input <- getInput
  return $ minimum $ (fuelToTarget2 input <$> candidates input)
