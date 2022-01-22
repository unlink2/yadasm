module Yadasm.Symbol where

data Symbol =
  Symbol { address :: Integer, name :: String, shadow :: Bool, order :: Int }
  deriving (Show, Eq)

