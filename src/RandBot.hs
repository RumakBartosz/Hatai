module RandBot (chooseMove, stringifyMoves, translateMove, getMove)
       where

import ParseMap
import System.IO
import System.Random
import HataiTypes

chooseMove :: [String] -> IO String
chooseMove moves = do
  index <- randomRIO (0, length moves - 1)
  return $ moves !! index

translateMove :: Move -> String
translateMove UP    = "up"
translateMove DOWN  = "down"
translateMove LEFT  = "left"
translateMove RIGHT = "right"

stringifyMoves :: [Move] -> [String]
stringifyMoves [] = []
stringifyMoves (x:xs) = translateMove x : stringifyMoves xs

getMove :: Color -> [String] -> IO()
getMove color tronMap = (chooseMove $ stringifyMoves $ getAllAvailableMoves color tronMap) >>= putStrLn
