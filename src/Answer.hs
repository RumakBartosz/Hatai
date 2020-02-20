module Answer
  ( colorAssignment, colorAssignAnswer, answerProtocol, repeatMoveAnswer
  ) where

data Color = Red
           | Blue
           deriving (Eq, Show)

colorAssignment :: String -> Color
colorAssignment x
    | x == "color red" = Red
    | x == "color blue" = Blue
    | otherwise = error "invalid color assignment message"

interfaceDiscoveryAnswer :: String -> IO()
interfaceDiscoveryAnswer x
    | x == "tbi" = putStrLn "tbi ok"
    | otherwise = putStrLn "tbi n/a"

versionNumberAnswer :: String -> IO()
versionNumberAnswer x
    | x == "tbi v1" = putStrLn "tbi v1 ok"
    | otherwise = putStrLn "tbi v1 n/a"

colorAssignAnswer :: Color -> IO()
colorAssignAnswer x
    | x == Red =  putStrLn "color ok"
    | x == Blue = putStrLn "color ok"
    | otherwise = putStrLn "color n/a"

moveAnswer :: Color -> String -> IO()
moveAnswer color move
    | take 4 move == "move" && color == Red = putStrLn "up"
    | take 4 move == "move" && color == Blue = putStrLn "down"
    | otherwise = putStrLn "move n/a"

repeatMoveAnswer :: Color -> IO()
repeatMoveAnswer color = do
    move <- getLine
    moveAnswer color move
    repeatMoveAnswer color

answerProtocol :: String -> IO()
answerProtocol message
    | take 5 message == "tbi v" = versionNumberAnswer message
    | take 3 message == "tbi" = interfaceDiscoveryAnswer message
    | otherwise = putStrLn "general message error"

-- exitReaction :: IO()
