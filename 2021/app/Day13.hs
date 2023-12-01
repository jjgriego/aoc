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
import qualified Data.Graph.Inductive as G

data FoldIns = AlongX Int
             | AlongY Int
             deriving Show

data Input = Input { inputDots :: [(Int, Int)]
                   , inputFolds :: [FoldIns]
                   } deriving Show

parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s Input
parser_input = Input <$> (endBy1 coord newline) <*> (newline *> endBy1 foldIns newline)
  where
    space = L.space space1 mzero mzero
    lexeme = L.lexeme space
    symbol = L.symbol space
    coord = (,) <$> L.decimal <*> (char ',' *> L.decimal)
    foldIns = symbol "fold along" *> ((AlongX <$> (symbol "x=" *> L.decimal)) <|>
                                      (AlongY <$> (symbol "y=" *> L.decimal)))

getInput = do
  txt <- T.readFile "inputs/13.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"

initial (Input dots _) = S.fromList dots

flip1 x lx
  | x < lx    = x
  | x == lx   = error "ahh"
  | otherwise = lx - (x - lx)

doFold (AlongX lx) = S.map (\(x, y) -> (flip1 x lx, y))
doFold (AlongY ly) = S.map (\(x, y) -> (x, flip1 y ly))

soln = do
  input <- getInput
  return $ S.size $ doFold (head $ inputFolds input) $ initial input

soln2 = do
  input <- getInput
  let result = foldl (flip doFold) (initial input) (inputFolds input)
      xs = S.map fst result
      ys = S.map snd result
      max_x  = maximum xs
      max_y  = maximum ys
      min_x  = minimum xs
      min_y  = minimum ys
      sx = max_x - min_x + 1
  putStrLn $ show (min_y, max_y)
  forM_ [min_y .. max_y] $ \y -> do
    let line = flip execState (take sx $ repeat ' ') $ do
          forM_ [min_x .. max_x] $ \x -> do
            if S.member (x, y) result
              then ix (x - min_x) .= '#'
              else return ()
    putStrLn line

