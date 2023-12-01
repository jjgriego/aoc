{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day06 where

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
  txt <- T.readFile "inputs/06.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"

type Fish = [Int]

initialFish = [ 0, 0, 0, 0
              , 0, 0, 0, 0, 0]

addFishWithTimer :: Int -> Fish -> Fish
addFishWithTimer n = (ix n +~ 1)

fish :: [Int] -> Fish
fish timers = foldr addFishWithTimer initialFish timers

simulate :: Fish -> Fish
simulate = execState $ do
  fishSpawning <- gets (!!0)
  modify (\xs -> (tail xs) ++ [fishSpawning])
  ix 6 += fishSpawning
  return ()

countFish :: Fish -> Int
countFish = foldr (+) 0

fishAfter :: Int -> Fish -> Int
fishAfter n initial = countFish $ (iterate simulate initial) !! n

soln = do
  input <- fish <$> getInput
  return $ fishAfter 80 input

soln2 = do
  input <- fish <$> getInput
  return $ fishAfter 256 input



