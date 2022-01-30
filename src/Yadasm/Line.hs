module Yadasm.Line where

import qualified Yadasm.Context as C
import qualified Yadasm.Symbol as S

type NodeResult = Maybe ([CodeWord], [S.Symbol])

-- attributes for code words
data Attrs = Std
           | NewLine
           | NewLineConnector Int
           | Connector Int
  deriving (Show, Eq)

-- a single token of a line of code
-- if a codeword is a new line token \n is inserted and it is 
-- prefixed when converted to string 
data CodeWord =
  CodeWord { text :: String, size :: Int, raw :: Integer, attr :: Attrs }
  deriving (Show, Eq)

defaultCodeWord :: CodeWord
defaultCodeWord = CodeWord { text = "", size = 0, raw = 0, attr = Std }

dup :: Int -> [a] -> [a]
dup n str = concat $ replicate n str

getConnectorN :: Attrs -> Int
getConnectorN (NewLineConnector n) = n
getConnectorN (Connector n) = n
getConnectorN _ = 1

dupConnector :: CodeWord -> [a] -> [a]
dupConnector v = dup (getConnectorN (attr v))

totalSize :: [CodeWord] -> Int
totalSize = foldl calcSize 0
  where
    calcSize :: Int -> CodeWord -> Int
    calcSize prev v = size v + prev

wordToString :: String -> String -> String -> CodeWord -> String
wordToString nl connector prev v = matchAttr (attr v)
  where
    appendNewLine = prev ++ nl ++ dupConnector v connector ++ text v

    matchAttr NewLine = appendNewLine
    matchAttr (NewLineConnector n) = appendNewLine
    matchAttr attr = prev ++ text v

resultToString :: C.Context -> NodeResult -> Maybe String
resultToString = resultToString' (wordToString "\n") S.symbolToString "" ""

resultToString' :: (String -> String -> CodeWord -> String)
                -> (String -> S.Symbol -> String)
                -> String
                -> String
                -> C.Context
                -> NodeResult
                -> Maybe String
resultToString'
  wordToString
  symbolToString
  connector
  end
  ctx
  (Just (word:words, symbols)) = Just
  (foldl symbolToString "" (getSyms syms)
   ++ dupConnector word connector
   ++ foldl (wordToString connector) "" (word:words)
   ++ end)
  where
    syms = C.getSymbolAt ctx (C.address ctx)

    getSyms :: Maybe [S.Symbol] -> [S.Symbol]
    getSyms Nothing = []
    getSyms (Just syms) = syms
resultToString' wordToString symbolToString connector end ctx _ = Nothing
