module Yadasm.Definition where

data Definition = Definition { text :: String, value :: Integer, size :: Int }
  deriving (Show, Eq)

defaultDefinition = Definition { text = "", value = 0, size = 1 }
