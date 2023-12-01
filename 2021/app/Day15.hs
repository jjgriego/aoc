{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
module Day15 where

import GHC.Generics
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
import Control.DeepSeq
import Data.Fix
import Text.Megaparsec.Char
import System.ProgressBar
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
import qualified Data.Heap as H

data Input = Input { inputTemplate :: String
                   , inputPairs :: [((Char, Char), Char)]
                   } deriving Show

parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s [[Int]]
parser_input = endBy1 (many (readDigit <$> digitChar)) newline
  where
    readDigit c = read [c]

getInput = do
  txt <- T.readFile "inputs/15.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"

loadInput input = (n, m, weights)
  where
    n = length (head input)
    m = length input

    arrBounds = ((0,0), (n-1, m-1))

    weights = array arrBounds [ ((x,y), (input !! y) !! x)
                              | x <- [0 .. n-1]
                              , y <- [0 .. m-1]
                              ]


data SearchState = SearchState { _frontier :: H.MinHeap (Int, Int, Int)
                               , _costs    :: Array (Int, Int) (Maybe Int)
                               } deriving (Show, Generic)
$(makeLenses ''SearchState)

dijkstra n m lookupWeight = do
    pbar <- liftIO $ newProgressBar defStyle 3.0 (Progress 0 (n * m) ())
    putStrLn (show (n, m))
    res <- (execStateT (impl pbar) initial)
    return (res ^. (costAt (n-1, m - 1)))
  where
    initial = SearchState (H.singleton (0, 0, 0)) (array ((0,0),(n-1,m-1)) [((x,y), Nothing) | x <- [0..n-1], y <- [0..m-1]])

    impl pbar = do
      costs %= force
      done <- uses (costAt (n - 1, m - 1)) isJust
      when (not done) $ do
        next <- uses frontier (force . H.viewHead)
        liftIO $ incProgress pbar 1
        case next of
          Nothing -> return ()
          Just (cost, x, y) -> do
            frontier %= fromJust . H.viewTail
            frontier %= H.filter (\(_, x0, y0) -> (x0, y0) /= (x, y))
            (costAt (x,y)) ?= cost
            forM_ [(x + 1, y)
                  ,(x - 1, y)
                  ,(x, y + 1)
                  ,(x, y - 1)] $ \(x2, y2) -> do
              bs <- uses costs bounds
              when (inRange bs (x2, y2)) $ do
                seen <- uses (costAt (x2, y2)) isJust
                when (not seen) $ do
                  let weight = lookupWeight (x2, y2)
                  frontier %= H.insert(cost + weight, x2, y2)
            impl pbar


    costAt coords = unsafeSingular (costs . ix coords)

soln = do
  (n, m, weights) <- loadInput <$> getInput
  dijkstra n m (weights!)

soln2 = do
    (n, m, weights) <- loadInput <$> getInput
    dijkstra (n * 5) (m * 5) (weightAt n m weights)
  where
    add a b = ((a + b - 1) `div` 9) + 1

    weightAt n m weights (x, y) = let (x', du) = divMod x n
                                      (y', dv) = divMod y m in
                                    (weights!(x',y')) `add` (du + dv)

    
  
