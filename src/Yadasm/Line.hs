module Yadasm.Line where

data CodeWord = CodeWord { text :: String, size :: Int, raw :: Integer }
  deriving (Show, Eq)

defaultCodeWord = CodeWord { text = "", size = 0, raw = 0 }

totalSize :: [CodeWord] -> Int
totalSize = foldl calcSize 0
  where
    calcSize :: Int -> CodeWord -> Int
    calcSize prev v = size v + prev
