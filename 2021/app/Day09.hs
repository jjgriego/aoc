{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Day09 where

import Debug.Trace
import Text.Megaparsec hiding (getInput)
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Semigroup
import Control.Monad.State
import Control.Monad.Writer
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
import Data.Array hiding (bounds)

parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s [[Int]]
parser_input = endBy1 (many digit) newline
  where
    digit = (\c -> read [c]) <$> oneOf ['0' .. '9']

getInput = do
  txt <- T.readFile "inputs/09.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"

data Grid a = Grid { gridDim :: (Int, Int)
                   , gridLookup :: Int -> Int -> Maybe a}

makeGrid :: [[a]] -> Grid a
makeGrid xs = Grid (n, m) at
  where
    n = length xs
    m = length (head xs)
    at i j
      | i < n && i >= 0 && j < m && j >= 0 = Just ((xs !! i) !! j)
      | otherwise                          = Nothing

findLocalMinima :: Grid Int -> [(Int, Int)]
findLocalMinima g = [ (i, j)
                    | i <- [0..(n - 1)]
                    , j <- [0..(m - 1)]
                    , all ((fromJust $ at i j)<) (neighbors i j)
                    ]
  where
    (n, m) = gridDim g
    at i j = gridLookup g i j
    neighbors i j = catMaybes [ at (i + 1) j
                              , at i (j + 1)
                              , at (i - 1) j
                              , at i (j - 1)]

assertAt g (i, j) = fromJust (gridLookup g i j)

soln = do
  grid <- makeGrid <$> getInput
  return $ sum $ (+1) <$> assertAt grid <$> findLocalMinima grid

findBasinSize g i j = S.size $ execState (impl i j) (S.empty)
  where
    lookup i j = gridLookup g i j
    indexedAt i j = ((i, j),) <$> lookup i j
    neighbors i j = catMaybes [ indexedAt (i + 1) j
                              , indexedAt i (j + 1)
                              , indexedAt (i - 1) j
                              , indexedAt i (j - 1)]

    impl i j = do
      reached <- uses (at (i, j)) isJust
      if reached
        then return ()
        else case lookup i j of
               Nothing -> return ()
               Just n -> do
                 modify (S.insert (i, j))
                 forM_ (fst <$> filter (\(coord, m) -> m > n && m /= 9) (neighbors i j)) (uncurry impl)

  {-
    impl i j visited
      | S.member (i, j) visited = visited
      | otherwise = case at i j of
                      Nothing -> visited
                      Just n ->
                        let steps = uncurry impl . fst <$> filter (\(coord, m) -> m > n && m /= 9) (neighbors i j) in
                          foldl (.) id steps (S.insert (i, j) visited)
-}

soln2 = do
  grid <- makeGrid <$> getInput
  return $ product $ take 3 $ reverse $ sort $ uncurry (findBasinSize grid) <$> findLocalMinima grid

