{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Day02 where

import Text.Megaparsec hiding (getInput)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.String

data Command = Forward Int
             | Up Int
             | Down Int
             deriving (Show, Eq)

parse_command :: (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s Command
parse_command =
    (Forward <$> (symbol "forward" *> num)) <|>
    (Up <$> (symbol "up" *> num)) <|>
    (Down <$> (symbol "down" *> num))
  where
    space = L.space space1 mzero mzero
    lexeme = L.lexeme space
    symbol = L.symbol space

    num = lexeme L.decimal

getInput :: IO [ Command ]
getInput = do
  txt <- T.readFile "inputs/02.txt"
  let result = parse (many parse_command) "inputs/02.txt" txt
  case result of
    Right comm -> return comm
    Left _ -> error "getInput"


apply (Forward x) = do
  _1 <+= x
  aim <- use _3
  _2 <+= aim * x
  return ()
apply (Up x) = void (_3 <-= x)
apply (Down x) = void (_3 <+= x)

run cs = forM cs $ apply

soln = do
  input <- getInput
  let (horiz, depth, _) = execState (run input) (0, 0, 0)
  return (horiz * depth)


