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
module Day11 where

import Debug.Trace
import Text.Megaparsec hiding (getInput)
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Semigroup hiding (First, getFirst)
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

parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s [[Int]]
parser_input = endBy1 (many digit) newline
  where
    digit = (\c -> read [c]) <$> oneOf ['0' .. '9']

getInput = do
  txt <- T.readFile "inputs/11.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"

newtype Grid a = Grid { _unGrid :: Array (Int, Int) a }
$(makePrisms ''Grid)

instance Functor Grid where
  fmap f (Grid a) = Grid (f <$> a)


type instance (Index (Grid a)) = (Int, Int)
type instance (IxValue (Grid a)) = a

instance Ixed (Grid a) where
  ix i = _Grid . ix i

instance Foldable Grid where
  foldMap f (Grid a) = foldMap f a

instance Traversable Grid where
  traverse = _Grid . traverse


gridSize :: Grid a -> (Int, Int)
gridSize (Grid a) = (n, m)
  where
    ((0, 0), (n, m)) = bounds a

gridLookup :: Grid a -> Int -> Int -> Maybe a
gridLookup g@(Grid a) i j
    | i < n && i >= 0 && j < m && j >= 0 = Just (a ! (i, j))
    | otherwise                          = Nothing
  where
    (n, m) = gridSize g

gridElems :: Grid a -> [(Int, Int, a)]
gridElems (Grid a) = fmap (\(i,j) -> (i,j,a!(i,j))) $ range $ bounds a

instance Show a => Show (Grid a) where
  show g = concat $ intersperse "\n" $ fmap showLine $ [0..(n - 1)]
    where
      (n,m) = gridSize g
      lookup i j = fromJust $ gridLookup g i j
      showLine i = concat $ intersperse ", " $ fmap (show . lookup i) [0..(m - 1)]

makeGrid :: [[a]] -> Grid a
makeGrid xs = Grid (array bounds [ ((i,j), ((xs !! i) !! j)) | (i, j) <- range bounds ])
  where
    bounds = ((0,0), (n - 1, m - 1))
    n = length xs
    m = length (head xs)

data GameState = GameState { _flashCount :: Int
                           , _stepCount :: Int
                           , _grid :: Grid (Maybe Int) }
$(makeLenses ''GameState)

tick = incr >> flash >> restore
incr = grid %= (fmap (fmap (+1)))
restore = grid %= (fmap restoreOne)
  where
    restoreOne Nothing = Just 0
    restoreOne (Just x) = Just x
flash = do
  g <- use grid
  case getFirst $ mconcat [ case a of
                              (Just n)
                                | n > 9 -> First $ Just (i, j)
                              _ -> First $ Nothing
                          | (i, j, a) <- gridElems g
                          ] of
    Just (i,j) -> do
      flashCount += 1
      (grid . ix (i,j)) .= Nothing
      forM_ [-1..1] $ \di -> do
        forM_ [-1..1] $ \dj -> do
          (grid . ix (i + di, j + dj)) %= fmap (+1)
      flash
    Nothing -> return ()

findFlashSync = do
  stepCount += 1
  incr
  flash
  g <- use grid
  if allOf traverse isNothing g
    then use stepCount
    else restore >> findFlashSync

soln = do
  input <- makeGrid <$> getInput
  let st = execState (forM_ [1..100] (const tick)) (GameState 0 0 (Just <$> input))
  return (_flashCount st)

soln2 = do
  input <- makeGrid <$> getInput
  let count = evalState (findFlashSync) (GameState 0 0 (Just <$> input))
  return count
