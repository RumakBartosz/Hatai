module Main where

import Answer

main :: IO ()
main = do
    message <- getLine
    answerProtocol message
    versionMessage <- getLine
    answerProtocol versionMessage
    colorMessage <- getLine
    let headColor = colorAssignment colorMessage
    colorAssignAnswer headColor
    repeatMoveAnswer headColor
