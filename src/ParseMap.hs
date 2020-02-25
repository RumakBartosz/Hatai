module ParseMap
    ( parseMap, getRedHeadPosition, getBlueHeadPosition
    ) where

import Data.List.Split
import Data.List
import Text.Read
import Data.Maybe

breakOnSlash :: String -> [String]
breakOnSlash = splitOn "/"

numberToSpaces :: Int -> String
numberToSpaces 0 = ""
numberToSpaces x = ' ' : numberToSpaces (x - 1)

numberParser :: String -> String
numberParser [] = []
numberParser [x] = [x]
numberParser (x:y:xs)
  | isJust (readMaybe [x, y] :: Maybe Int) = numberToSpaces (fromJust (readMaybe [x, y] :: Maybe Int)) ++ numberParser xs
numberParser (x:xs)
  | isJust (readMaybe [x] :: Maybe Int) = numberToSpaces (fromJust (readMaybe [x] :: Maybe Int)) ++ numberParser xs
  | otherwise = x : numberParser xs

parseMap :: String -> [String]
parseMap x = breakOnSlash $ numberParser x

getHorizontalValueOfMark :: Char -> [String] -> Int
getHorizontalValueOfMark _ [[]] = -100
getHorizontalValueOfMark _ [] = -100
getHorizontalValueOfMark c (x:xs)
  | isJust(elemIndex c x) = 0
  | otherwise = 1 + getHorizontalValueOfMark c xs

getVerticalValueOfMark :: Char -> [String] -> Int
getVerticalValueOfMark _ [[]] = -100
getVerticalValueOfMark _ [] = -100
getVerticalValueOfMark c (x:xs)
  | isJust(elemIndex c x) = fromJust $ elemIndex c x
  | otherwise = getVerticalValueOfMark c xs

getRedHeadPosition :: [String] -> (Int, Int)
getRedHeadPosition tronMap = (getVerticalValueOfMark 'R' tronMap, getHorizontalValueOfMark 'R' tronMap)

getBlueHeadPosition :: [String] -> (Int, Int)
getBlueHeadPosition tronMap = (getVerticalValueOfMark 'B' tronMap, getHorizontalValueOfMark 'B' tronMap)
