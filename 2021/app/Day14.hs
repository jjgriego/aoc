{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Day14 where

import Debug.Trace
import Text.Megaparsec hiding (getInput)
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Semigroup hiding (First, getFirst)
import Control.Applicative hiding (some, many)
import Control.Monad.State
import Control.Monad.Writer hiding (First, getFirst)
import Control.Monad.Cont
import Control.Monad.Trans.Maybe
import Control.Monad.Logic
import Control.Monad.IO.Class
import Data.Fix
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Foldable
import Data.String
import Data.Maybe
import Data.List
import Data.Bits
import Data.Array
import Data.Char
import Data.Tuple
import Data.Function (on)
import qualified Data.Graph.Inductive as G

data Input = Input { inputTemplate :: String
                   , inputPairs :: [((Char, Char), Char)]
                   } deriving Show

parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s Input
parser_input = Input <$> (template <* newline <* newline) <*> endBy1 rule newline
  where
    space = L.space space1 mzero mzero
    lexeme = L.lexeme space
    symbol = L.symbol space

    template = some elem
    elem = upperChar
    rule = (,) <$> lexeme ((,) <$> elem <*> elem) <*> (symbol "->" *> elem)

getInput = do
  txt <- T.readFile "inputs/14.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"

applyRules rules pairs = foldl' (M.unionWith (+)) M.empty (fmap evalPair $ M.toList pairs)
  where
    evalPair (pair@(c1,c3), count) = case M.lookup pair rules of
                                       Just c2 -> M.fromList [((c1, Just c2), count), ((Just c2, c3), count)]
                                       Nothing -> M.singleton pair count

initial input = M.fromListWith (+) $ fmap (,1) $ zip str (tail str)
  where
    str = [Nothing] ++ (Just <$> inputTemplate input) ++ [Nothing]



elemCounts pairs = M.map (`div` 2) $ M.mapKeys fromJust $ M.delete Nothing $ foldl' (M.unionWith (+)) M.empty (fmap splitPair $ M.toList pairs)
  where
    splitPair ((a, b), count)
      | a == b    = M.singleton a (count * 2)
      | otherwise = M.fromList [(a, count), (b, count)]

mkSoln n = do
  input <- getInput
  let f = applyRules (M.mapKeys (\(a, b) -> (Just a, Just b)) $ M.fromList $ inputPairs input)
  let res = (iterate f (initial input)) !! n
  let counts = M.toList $ elemCounts res
  let (minelem, minq) = minimumBy (compare `on` snd) counts
  let (maxelem, maxq) = maximumBy (compare `on` snd) counts
  return $ maxq - minq


soln = mkSoln 10
soln2 = mkSoln 40
  
  
  
