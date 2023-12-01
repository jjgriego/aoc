{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE PartialTypeSignatures #-}
module Day08 where

import Debug.Trace
import Text.Megaparsec hiding (getInput)
import Control.Lens
import Control.Monad
import Data.Monoid
import Data.Semigroup
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.Cont
import Control.Monad.Logic
import Control.Monad.IO.Class
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

data Line = Line { lineDigits :: [S.Set Char]
                 , lineOut    :: [S.Set Char]} deriving (Show)

  
parser_input :: forall s. (Stream s, Token s ~ Char, IsString (Tokens s)) => Parsec () s [Line]
parser_input = many $ do
    digits <- forM [1..10] $ \_ -> do
      lexeme signal
    symbol "|"
    outSignals <- forM [1..4] $ \_ -> lexeme signal
    return $ Line digits outSignals
    
  where
    space = L.space space1 mzero mzero
    lexeme = L.lexeme space
    symbol = L.symbol space

    code = oneOf ['a' .. 'g']
    signal = S.fromList <$> some code

getInput :: IO _
getInput = do
  txt <- T.readFile "inputs/08.txt"
  let result = parse parser_input "<input>" txt
  case result of
    Right input -> return input
    Left _ -> error "getInput"


soln = do
    input <- getInput
    return $ sum $ countSpecialDigits <$> input
  where
    countSpecialDigits (Line digits out) = (sum $ countSpecialDigits' <$> out)
    countSpecialDigits' s
      | (n == 2) || (n == 3) || (n == 4) || (n == 7) = 1
      | otherwise = 0
      where
        n = S.size s

-- maps each signal to possible segments it corresponds to
type InferenceState = M.Map Char (S.Set Char)
type InferT m a = (StateT InferenceState (LogicT m)) a

-- refine :: Char -> (S.Set Char -> S.Set Char) -> Infer ()
refine c modify = do
    Just before <- use (at c)
    (at c) <%= Just . modify . fromJust
    Just remaining <- use (at c)
    case S.toList remaining of
      [] -> empty
      [k]
        | (S.size before) > (S.size remaining) -> do
          forM_ ['a'..'g'] $ \k2 -> do
            if k2 == c
              then return ()
              else refine k2 (flip S.difference (S.singleton k))
        | otherwise -> return ()
      _ -> return ()

allSignals = S.fromList ['a' .. 'g']
allSegments = S.fromList ['A' .. 'G']

startInferenceState = M.fromList [(sig, allSegments) | sig <- S.toList allSignals]


mustBeOneOf segments signals = forM_ signals $ \c -> refine c (S.intersection segments)

exclusive segments signals = do
  -- liftIO $ putStrLn $ "exclusive" ++ (show (segments, signals))
  mustBeOneOf segments signals
  mustBeOneOf segmentsInv signalsInv
  where
    segmentsInv = S.difference allSegments segments
    signalsInv = S.difference allSignals signals
exclusive' segments signals = exclusive (S.fromList segments) signals


realDigits = S.fromList <$> [ "ABCEFG"
                            , "CF"
                            , "ACDEG"
                            , "ACDFG"
                            , "BDCF"
                            , "ABDFG"
                            , "ABDEFG"
                            , "ACF"
                            , "ABCDEFG"
                            , "ABCDFG"
                            ]

check :: [(S.Set Char)] -> InferT IO [(Char, Char)]
check digits = do
    -- prioritize the obvious ones
    forM_ digits checkObviousDigit
    forM_ digits pickAssignment
    forM (S.toList allSignals) $ \sig -> do
      Just st <- use (at sig)
      foldr (<|>) empty $ (pure . (sig, ) <$> S.toList st)
  where
    checkObviousDigit digit
      | (S.size digit) == 2 = exclusive' "CF" digit
      | (S.size digit) == 3 = exclusive' "ACF" digit
      | (S.size digit) == 4 = exclusive' "BDCF" digit
      | otherwise           = return ()

    -- pick _some_ displayed digit for this signal to correspond to
    -- (i.e. just guess--the backtracking logic saves us)
    pickAssignment digit = do
      reachableSegments <- reachableSegmentsFor digit
      let possibilities = filter (compatible digit reachableSegments) realDigits
      segments <- foldr (<|>) empty (pure <$> possibilities)
      exclusive segments digit

    compatible signals reachable digit = (S.size signals == S.size digit) && (S.isSubsetOf digit reachable)

    -- find the upper limit (union) of all segments this signal _could_ illuminate
    reachableSegmentsFor digit = do
      m <- get
      return $ S.unions $ S.map (fromJust . flip M.lookup m) digit

readDigits mapping digits = convert (readDigit <$> S.map lookup <$> digits)
  where
    lookup sig = fromJust $ M.lookup sig (M.fromList mapping)
    readDigit lit = fromJust $ fst <$> find (\(d, segments) -> segments == lit) (zip [0..9] realDigits)
    convert ds = foldl (\x d -> (x * 10) + d) 0 ds

soln2 = do
  input <- getInput
  outs <- forM input $ \line -> do
    mappings <- observeAllT $ evalStateT (check (lineDigits line)) startInferenceState
    case mappings of
      [mapping] -> return $ readDigits mapping (lineOut line)
      _ -> error "asdf"
  return $ sum outs
