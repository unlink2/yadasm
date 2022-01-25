module Yadasm.Symbol where

data Symbol =
  Symbol { address :: Integer, name :: String, shadow :: Bool, order :: Int }
  deriving (Show, Eq)

defaultSymbol = Symbol { address = 0, name = "", shadow = False, order = 0 }

symbolToString' :: String -> String -> String -> Symbol -> String
symbolToString' prefix postfix prev v
  | shadow v = ""
  | otherwise = prev ++ prefix ++ name v ++ postfix

symbolToString :: String -> Symbol -> String
symbolToString = symbolToString' "" ":\n"
