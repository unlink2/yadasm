module Yadasm.Symbol where

data Attrs = Std
           | NewLine
           | Shadow
  deriving (Show, Eq)

data Symbol =
  Symbol { address :: Integer, name :: String, order :: Int, attr :: Attrs }
  deriving (Show, Eq)

defaultSymbol = Symbol { address = 0, name = "", order = 0, attr = NewLine }

symbolToString' :: String -> String -> String -> String -> Symbol -> String
symbolToString' nl prefix postfix prev v
  | attr v == Shadow = ""
  | attr v == NewLine = prev ++ prefix ++ name v ++ postfix ++ nl
  | otherwise = prev ++ prefix ++ name v ++ postfix

symbolToString :: String -> Symbol -> String
symbolToString = symbolToString' "\n" "" ":"
