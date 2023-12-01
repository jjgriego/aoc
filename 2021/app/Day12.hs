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

parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s [(Room, Room)]
parser_input = endBy1 ((,) <$> (mkRoom <$> label) <*> (mkRoom <$> (char '-' *> label))) newline
  where
    digit = (\c -> read [c]) <$> oneOf ['0' .. '9']
    label = some letterChar

getInput = do
  txt <- T.readFile "inputs/12.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"

data Room = Small String
          | Big String
          deriving (Show, Eq, Ord)

mkRoom name
  | isUpper (head name) = Big name
  | otherwise           = Small name

caveGraph :: G.DynGraph gr => [(Room, Room)] -> (G.Node, G.Node, gr Room ())
caveGraph pairs = flip evalState (M.empty, 0 :: Int) $ do
    edges <- forM pairs $ \(r1, r2) -> do
      r1_id <- getId r1
      r2_id <- getId r2
      return (r1_id, r2_id, ())
    nodes <- fmap swap <$> uses _1 M.toList
    start <- fromJust <$> use (_1 . at (Small "start"))
    end <- fromJust <$> use (_1 . at (Small "end"))
    return (start, end, G.mkGraph nodes edges)
  where
    getId r = do
      id <- use (_1 . at r)
      case id of
        Just id' -> return id'
        Nothing  -> do
          next <- use _2
          _2 += (1 :: Int)
          (_1 . at r) .= Just next
          return next

path :: (Monad m, Alternative m, G.Graph gr)
     => ([(Room, G.Node)] -> m (Room, G.Node))
     -> gr Room ()
     -> G.Node
     -> G.Node
     -> [Room]
     -> m [Room]
path pick gr n end prev
    | n == end  = return $ reverse prev
    | otherwise = do
      (label, next) <- pick $ labels $ G.neighbors gr n
      path pick gr next end (label:prev)
  where
    labels ns = fmap (\n -> (fromJust $ G.lab gr n, n)) ns

soln = do
    (start, end, gr) <- caveGraph @ G.Gr <$> getInput
    return $ length $ observeAll (evalStateT (path pick gr start end []) S.empty)
  where
    pick options = do
      opts <- filterM shouldVisit options
      (label, next) <- foldl (<|>) empty $ fmap pure opts
      modify (S.insert next)
      return (label, next)
    shouldVisit (Big _, n) = return $ True
    shouldVisit (Small _, n) = not <$> gets (S.member n)

data SearchState = SearchState { _visited :: S.Set G.Node
                               , _twiceUsed :: Bool}
$(makeLenses ''SearchState)


soln2 = do
    (start, end, gr) <- caveGraph @ G.Gr <$> getInput
    return $ length $ observeAll (evalStateT (path pick gr start end []) (SearchState S.empty False))
  where
    pick options = do
      (label, next) <- foldl (<|>) empty $ fmap pure options
      case label of
        (Big _) -> return (label, next)
        (Small name)
          | name == "start" -> empty
          | name == "end"   -> return (label, next)
          | otherwise       -> do
            seen <- uses visited (S.member next)
            twice <- use twiceUsed
            case (seen, twice) of
              (True, True) -> empty
              (True, False) -> do
                twiceUsed .= True
                return (label, next)
              (False, _) -> do
                visited %= S.insert next
                return (label, next)

