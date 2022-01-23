module Yadasm.Context where

import           Yadasm.Symbol (Symbol)
import qualified Yadasm.Symbol
import           Data.HashMap.Lazy as HashMap
import           Data.Maybe (isNothing)

-- the context is a data structure holding information 
-- such as the current address, a map of symbols etc
data Context = Context { symbols :: HashMap.HashMap Integer [Symbol]
                       , address :: Integer
                       , endAddress :: Integer
                       }
  deriving (Show, Eq)

defaultContext =
  Context { symbols = HashMap.fromList [], address = 0, endAddress = 0 }

isInAddrRange :: Context -> Integer -> Bool
isInAddrRange ctx addr = address ctx <= addr && endAddress ctx > addr

addSymbol :: Context -> Symbol -> Context
addSymbol ctx symbol =
  ctx { symbols = HashMap.insert addr (symbol:members) syms }
  where
    syms = symbols ctx

    addr = Yadasm.Symbol.address symbol

    members = HashMap.lookupDefault [] addr syms

getSymbolFrom :: Maybe [Symbol] -> Maybe Symbol
getSymbolFrom (Just members) = Just $ head members
getSymbolFrom Nothing = Nothing

getSymbolAt :: Context -> Integer -> Maybe Symbol
getSymbolAt ctx addr = getSymbolFrom members
  where
    members = HashMap.lookup addr (symbols ctx)

advance :: Integer -> Context -> Context
advance by ctx = ctx { address = address ctx + by }
