{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Day10 where

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

parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s [String]
parser_input = endBy1 (many $ oneOf @[] "()[]<>{}") newline

getInput = do
  txt <- T.readFile "inputs/10.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"


data ScanResult = Good
                | Illegal Char
                | Incomplete [Char]
                deriving Show

scan [] [] = Good
scan stk@(s : ss) (c : cs)
    | elem @[] c "<[{(" = scan (c : stk) cs
    | matches s c   = scan ss cs
    | otherwise = Illegal c
  where
    matches '[' ']' = True
    matches '{' '}' = True
    matches '(' ')' = True
    matches '<' '>' = True
    matches _ _ = False
scan [] (c: cs)
    | elem @[] c "<[{(" = scan [c] cs
    | otherwise        = Illegal c
scan stk [] = Incomplete stk

score ')' = 3
score ']' = 57
score '}' = 1197
score '>' = 25137

soln = do
  input <- getInput
  return $ sum $ fmap score <$> catMaybes $
    fmap (\r -> case scan [] r of
                  Illegal c -> Just c
                  _ -> Nothing)
         input


close '[' = ']'
close '{' = '}'
close '(' = ')'
close '<' = '>'

closeScore ')' = 1
closeScore ']' = 2
closeScore '}' = 3
closeScore '>' = 4

soln2 = do
  input <- getInput

  return $ median $ catMaybes $ (\line -> case scan [] line of
    Incomplete stk -> Just $ completionScore (fmap close stk)
    _ -> Nothing)
    <$> input
  where
    completionScore cs = foldl (\s c-> s * 5 + closeScore c) 0 cs

median xs = let n = length xs in
              (sort xs) !! (n `div` 2)
  
