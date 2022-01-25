module Yadasm.TestContext where

import           Test.HUnit
import           Yadasm.Context
import qualified Yadasm.Symbol as S
import           Data.HashMap.Lazy as HashMap

testContext =
  defaultContext { symbols = HashMap.fromList
                     [ ( 0x100
                       , [ S.defaultSymbol { S.address = 0x100, S.name = "2" }
                         , S.defaultSymbol { S.address = 0x100, S.name = "1" }])
                     , ( 0x101
                       , [S.defaultSymbol { S.address = 0x101, S.name = "3" }])]
                 }

tests =
  [ TestCase
      (assertBool
         "It should be in address range (lower bound)"
         (isInAddrRange defaultContext { address = 100, endAddress = 110 } 100))
  , TestCase
      (assertBool
         "It should be in address range"
         (isInAddrRange defaultContext { address = 100, endAddress = 110 } 105))
  , TestCase
      (assertBool
         "It should be in address range (upper bound)"
         (isInAddrRange defaultContext { address = 100, endAddress = 110 } 109))
  , TestCase
      (assertBool
         "It should be outside address range (less than)"
         (not
            (isInAddrRange
               defaultContext { address = 100, endAddress = 110 }
               99)))
  , TestCase
      (assertBool
         "It should be outside address range (greater than)"
         (not
            (isInAddrRange
               defaultContext { address = 100, endAddress = 110 }
               110)))
  , TestCase
      (assertEqual
         "It should add symbols"
         (addSymbol
            (addSymbol
               (addSymbol
                  defaultContext
                  S.defaultSymbol { S.address = 0x100, S.name = "1" })
               S.defaultSymbol { S.address = 0x100, S.name = "2" })
            S.defaultSymbol { S.address = 0x101, S.name = "3" })
         testContext)
  , TestCase
      (assertEqual
         "It should get firt symbol"
         (Just
            [ S.defaultSymbol { S.address = 0x100, S.name = "2" }
            , S.defaultSymbol { S.address = 0x100, S.name = "1" }])
         (getSymbolAt testContext 0x100))
  , TestCase
      (assertEqual
         "It should not find symbol"
         Nothing
         (getSymbolAt testContext 0x99))
  , TestCase
      (assertEqual
         "It should add flags"
         (HashMap.fromList [("test", "123")])
         (flags (setFlag "test" "123" testContext)))
  , TestCase
      (assertEqual
         "It should remove flags"
         (HashMap.fromList [])
         (flags
            (unsetFlag
               "test"
               testContext { flags = HashMap.fromList [("test", "123")] })))
  , TestCase
      (assertEqual
         "It should not remove flags that don't exist"
         (HashMap.fromList [("test", "123")])
         (flags
            (unsetFlag
               "atest"
               testContext { flags = HashMap.fromList [("test", "123")] })))
  , TestCase
      (assertEqual
         "It should get flags"
         (Just "123")
         (lookupFlag
            "test"
            testContext { flags = HashMap.fromList [("test", "123")] }))
  , TestCase
      (assertEqual
         "It should not get flags that don't exist"
         Nothing
         (lookupFlag
            "atest"
            testContext { flags = HashMap.fromList [("test", "123")] }))]
