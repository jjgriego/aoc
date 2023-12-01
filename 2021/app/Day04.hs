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

import Text.Megaparsec hiding (getInput)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Monad.IO.Class
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Foldable
import Data.String
import Data.Maybe
import Data.List
import Data.Bits
import Data.Array

data BingoBoard a = BingoBoard (Array (Int, Int) a)
  deriving (Show, Eq, Functor, Foldable, Traversable)

makeBoard xs
  | length xs == 25 = Just $ BingoBoard $ listArray ((1,1),(5,5)) xs
  | otherwise = Nothing


data Input = Input { inputSequence :: [Int]
                   , inputBoards :: [BingoBoard Int]
                   } deriving (Show)




parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s Input
parser_input = do
    calls <- sepBy1 num (char ',')
    newline >> newline
    boards <- many board
    return (Input calls boards)
  where
    board :: Parsec () s (BingoBoard Int)
    board = do
      seq <- forM [1..25] $ \_ -> lexeme num
      let Just b = makeBoard seq
      return b

    space = L.space space1 mzero mzero
    lexeme = L.lexeme space

    num :: Parsec () s Int
    num = L.decimal


markBoard :: Int -> BingoBoard (Int, Bool) -> BingoBoard (Int, Bool)
markBoard needle board = mark <$> board
  where
    mark (x, set)
      | needle == x = (x, True)
      | otherwise   = (x, set)


boardWins (BingoBoard board) = winsColumn || winsRow -- || winsDiag
  where
    winsColumn = flip any [1..5] $ \i -> flip all [1..5] (marked i)
    winsRow = flip any [1..5] $ \i -> flip all [1..5] (flip marked i)
    marked :: Int -> Int -> Bool
    marked n m = snd (board ! (n,m))

scoreBoard (BingoBoard board) = sum (scoreCell <$> board)
  where
    scoreCell (n, False) = n
    scoreCell (_, True) = 0


getInput :: IO Input
getInput = do
  txt <- T.readFile "inputs/04.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right comm -> return comm
    Left _ -> error "getInput"

data Bingo = Bingo { _stNextNumbers :: [Int]
                   , _stBoards :: [BingoBoard (Int, Bool)]}
$(makeLenses ''Bingo)

step [] boards = return boards
step (n : upcoming) boards = (catMaybes <$> forM marked checkWin) >>= step upcoming
  where
    marked = fmap (markBoard n) boards

    checkWin b =
      if boardWins b
        then (tell $ [(n * scoreBoard b)]) >> return Nothing
        else return $ Just b

initial input = fmap (fmap (,False)) (inputBoards input)

winningSequence = do
  input <- getInput
  winningBoards <- execWriterT (step (inputSequence input) (initial input))
  return $ winningBoards

soln = head <$> winningSequence
soln2 = last <$> winningSequence
