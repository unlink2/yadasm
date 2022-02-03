module Yadasm.Error where

data Error = ParserError
           | OutOfDataError
  deriving (Show, Eq)
