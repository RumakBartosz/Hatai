module ParseMap
    ( parseMap, getRedHeadPosition, getBlueHeadPosition, getAllAvailableMoves
    ) where

import Data.List.Split
import Data.List
import Text.Read
import Data.Maybe
import Control.Lens
import HataiTypes

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
getRedHeadPosition tronMap = (getHorizontalValueOfMark 'R' tronMap, getVerticalValueOfMark 'R' tronMap)

getBlueHeadPosition :: [String] -> (Int, Int)
getBlueHeadPosition tronMap = (getHorizontalValueOfMark 'B' tronMap, getVerticalValueOfMark 'B' tronMap)

isMoveUpPossible :: Color -> [String] -> Bool
isMoveUpPossible Red tronMap = (tronMap ^? element (xValue - 1) . element yValue) == Just ' '
                               where
                                 (xValue, yValue) = getRedHeadPosition tronMap
isMoveUpPossible Blue tronMap = (tronMap ^? element (xValue - 1) . element yValue) == Just ' '
                                where
                                  (xValue, yValue) = getBlueHeadPosition tronMap

isMoveDownPossible :: Color -> [String] -> Bool
isMoveDownPossible Red tronMap = (tronMap ^? element (xValue + 1) . element yValue) == Just ' '
                               where
                                 (xValue, yValue) = getRedHeadPosition tronMap
isMoveDownPossible Blue tronMap = (tronMap ^? element (xValue + 1) . element yValue) == Just ' '
                                where
                                  (xValue, yValue) = getBlueHeadPosition tronMap

isMoveLeftPossible :: Color -> [String] -> Bool
isMoveLeftPossible Red tronMap = (tronMap ^? element xValue . element (yValue - 1)) == Just ' '
                               where
                                 (xValue, yValue) = getRedHeadPosition tronMap
isMoveLeftPossible Blue tronMap = (tronMap ^? element xValue . element (yValue - 1)) == Just ' '
                                where
                                  (xValue, yValue) = getBlueHeadPosition tronMap

isMoveRightPossible :: Color -> [String] -> Bool
isMoveRightPossible Red tronMap = (tronMap ^? element xValue . element (yValue + 1)) == Just ' '
                               where
                                 (xValue, yValue) = getRedHeadPosition tronMap
isMoveRightPossible Blue tronMap = (tronMap ^? element xValue . element (yValue + 1)) == Just ' '
                                where
                                  (xValue, yValue) = getBlueHeadPosition tronMap

getAllAvailableMoves :: Color -> [String] -> [Move]
getAllAvailableMoves color tronMap =
  [UP    | isMoveUpPossible color tronMap] ++
  [DOWN  | isMoveDownPossible color tronMap] ++
  [LEFT  | isMoveLeftPossible color tronMap] ++
  [RIGHT | isMoveRightPossible color tronMap]
