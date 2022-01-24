module Yadasm.Symbol where

data Symbol =
  Symbol { address :: Integer, name :: String, shadow :: Bool, order :: Int }
  deriving (Show, Eq)

defaultSymbol = Symbol { address = 0, name = "", shadow = False, order = 0 }

symbolToString :: String -> Symbol -> String
symbolToString prev v
  | shadow v = ""
  | otherwise = prev ++ name v ++ ":\n"
