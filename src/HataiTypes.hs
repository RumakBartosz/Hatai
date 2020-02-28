module HataiTypes
                   where

data Color = Red
           | Blue
           deriving (Eq, Show)

data Move = UP
          | DOWN
          | LEFT
          | RIGHT
          deriving (Eq, Show)
