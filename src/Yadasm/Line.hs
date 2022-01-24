module Yadasm.Line where

import           Yadasm.Context as C
import           Yadasm.Symbol as S

data CodeWord = CodeWord { text :: String, size :: Int, raw :: Integer }
  deriving (Show, Eq)

defaultCodeWord = CodeWord { text = "", size = 0, raw = 0 }

totalSize :: [CodeWord] -> Int
totalSize = foldl calcSize 0
  where
    calcSize :: Int -> CodeWord -> Int
    calcSize prev v = size v + prev

wordToString :: String -> CodeWord -> String
wordToString prev v = prev ++ text v

resultToString :: C.Context -> Maybe ([CodeWord], [S.Symbol]) -> Maybe String
resultToString = resultToString' wordToString S.symbolToString ""

resultToString' :: (String -> CodeWord -> String)
                -> (String -> S.Symbol -> String)
                -> String
                -> C.Context
                -> Maybe ([CodeWord], [S.Symbol])
                -> Maybe String
resultToString'
  wordToString
  symbolToString
  connector
  ctx
  (Just (words, symbols)) = Just
  (foldl S.symbolToString "" (getSyms syms)
   ++ connector
   ++ foldl wordToString "" words)
  where
    syms = C.getSymbolAt ctx (C.address ctx)

    getSyms :: Maybe [S.Symbol] -> [S.Symbol]
    getSyms Nothing = []
    getSyms (Just syms) = syms
resultToString' wordToString symbolToString connector ctx _ = Nothing
