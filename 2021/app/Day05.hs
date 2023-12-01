{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Day03 where

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


data Line = Line Int Int Int Int deriving Show
type Input = [Line]

parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s Input
parser_input = do
    many line
  where
    space = L.space space1 mzero mzero
    lexeme = L.lexeme space
    symbol = L.symbol space
    num = lexeme L.decimal
    line = Line <$> num <*> (symbol "," *> num) <*> (symbol "->" *> num) <*> (symbol "," *> num)

getInput :: IO Input
getInput = do
  txt <- T.readFile "inputs/05.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"

rasterizeLine (Line x0 y0 x1 y1)
    | x0 == x1 && y0 == y1 = [(x0, y0)]
    | otherwise            = step (x0 + sigma (x1 - x0)) (y0 + sigma (y1 - y0))
  where
    step x y = ( (x0, y0) : rasterizeLine (Line x y x1 y1))
    sigma x = case compare x 0 of
                LT -> -1
                EQ -> 0
                GT -> 1

allPoints lines = M.keys $ M.filter (>=2) counts
  where
    counts = M.unionsWith (+) maps
    maps = M.fromList . fmap (,1) <$> rasterizeLine <$> lines









