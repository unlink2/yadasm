module Yadasm.Context where

import           Yadasm.Symbol (Symbol)
import qualified Yadasm.Symbol
import           Data.HashMap.Lazy as HashMap
import           Data.Maybe (isNothing)
import qualified Yadasm.Definition as D
import           Yadasm.Error

-- the context is a data structure holding information 
-- such as the current address, a map of symbols etc
data Context =
  Context { symbols :: HashMap.HashMap Integer [Symbol]
          , address :: Integer
          , endAddress :: Integer
            -- flags may be used by plugins to signal 
            -- certain values to the caller if desired
          , flags :: HashMap.HashMap String String
          , definitions :: HashMap.HashMap Integer D.Definition
          , cerror :: Maybe Error
          }
  deriving (Show, Eq)

defaultContext =
  Context { symbols = HashMap.fromList []
          , address = 0
          , endAddress = 0
          , flags = HashMap.fromList []
          , definitions = HashMap.fromList []
          , cerror = Nothing
          }

isInAddrRange :: Context -> Integer -> Bool
isInAddrRange ctx addr = address ctx <= addr && endAddress ctx > addr

addSymbol :: Context -> Symbol -> Context
addSymbol ctx symbol =
  ctx { symbols = HashMap.insert addr (symbol:members) syms }
  where
    syms = symbols ctx

    addr = Yadasm.Symbol.address symbol

    members = HashMap.lookupDefault [] addr syms

getSymbolFrom :: Maybe [Symbol] -> Maybe [Symbol]
getSymbolFrom (Just members) = Just members
getSymbolFrom Nothing = Nothing

getSymbolAt :: Context -> Integer -> Maybe [Symbol]
getSymbolAt ctx addr = getSymbolFrom members
  where
    members = HashMap.lookup addr (symbols ctx)

advance :: Integer -> Context -> Context
advance by ctx = ctx { address = address ctx + by }

setFlag :: String -> String -> Context -> Context
setFlag name value ctx = ctx { flags = HashMap.insert name value (flags ctx) }

unsetFlag :: String -> Context -> Context
unsetFlag name ctx = ctx { flags = HashMap.delete name (flags ctx) }

lookupFlag :: String -> Context -> Maybe String
lookupFlag name ctx = HashMap.lookup name (flags ctx)

lookupDefinition :: Integer -> Context -> Maybe D.Definition
lookupDefinition value ctx = HashMap.lookup value (definitions ctx)
