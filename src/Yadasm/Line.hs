module Yadasm.Line where

data CodeWord = CodeWord { text :: String, size :: Int }
  deriving (Show, Eq)
