module Main where

import System.IO
import Answer

main :: IO ()
main = do
    message <- getLine
    answerProtocol message
    hFlush stdout
    versionMessage <- getLine
    answerProtocol versionMessage
    hFlush stdout
    colorMessage <- getLine
    let headColor = colorAssignment colorMessage
    colorAssignAnswer headColor
    hFlush stdout
    repeatMoveAnswer headColor
