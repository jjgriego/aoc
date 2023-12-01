{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Day03 where

import Text.Megaparsec hiding (getInput)
import Control.Lens
import Control.Monad
import Control.Monad.State
import Text.Megaparsec.Char
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.String
import Data.Maybe
import Data.List
import Data.Bits

getInput :: IO [[Bool]]
getInput = do
    content <- readFile "inputs/03.txt"
    return (parseBinary <$> lines content)

parseBinary = catMaybes . fmap parseDigit
  where
    parseDigit '0' = Just False
    parseDigit '1' = Just True
    parseDigit _ = Nothing

test = unlines [ "00100"
               , "11110"
               , "10110"
               , "10111"
               , "10101"
               , "01111"
               , "00111"
               , "11100"
               , "10000"
               , "11001"
               , "00010"
               , "01010"
               ]

modeBits :: [[Bool]] -> [Bool]
modeBits xs = assertModeWellDefined <$> flip extractBit xs <$> [0..(len - 1)]
  where
    len = length $ head xs
    assertModeWellDefined xs = let (Just b) = mode xs in b

extractBit n xs = (!!n) <$> xs

mode xs
  | true_count == false_count = Nothing
  | otherwise = Just (true_count > false_count)
    where
      true_count = length $ filter id xs
      false_count = length $ filter not xs

bitsToNum :: Bits n => [Bool] -> n
bitsToNum bs = impl $ reverse bs
  where
    impl [] = zeroBits
    impl (True:bs) = flip setBit 0 $ flip shiftL 1 $ impl bs
    impl (False:bs) = flip shiftL 1 $ impl bs

soln :: IO Integer
soln = do
  bits <- modeBits <$> getInput
  let gamma = bitsToNum bits
  let epsilon = bitsToNum $ fmap not bits
  return $ gamma * epsilon

whittle f xs = snd $ impl $ fmap (\x -> (x, x)) xs
  where
    impl [] = error "wtf"
    impl [x] = x
    impl xs = impl $ f xs

keepStarting b xs =
  catMaybes $ flip fmap xs $ \((n:ns), orig) ->
    if n == b
      then return $ (ns, orig)
      else Nothing

oxygenWhittler xs =
  case mode (fmap (head . fst) xs) of
    Just b -> keepStarting b xs
    Nothing -> keepStarting True xs

carbonWhittler xs =
  case mode (fmap (head . fst) xs) of
    Just b -> keepStarting (not b) xs
    Nothing -> keepStarting False xs

soln2 :: IO Integer
soln2 = do
  bits <- getInput
  return $ (bitsToNum $ whittle carbonWhittler bits) * (bitsToNum $ whittle oxygenWhittler bits)







