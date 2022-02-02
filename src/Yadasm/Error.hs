module Yadasm.Error where

data Error = ParserErrir
           | OutOfDataError
  deriving (Show, Eq)
