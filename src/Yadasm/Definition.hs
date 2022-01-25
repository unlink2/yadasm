module Yadasm.Definition where

data Definition = Definition { text :: String, value :: Integer }
  deriving (Show, Eq)

defaultDefinition = Definition { text = "", value = 0 }
