module Yadasm.TestLine where

import           Test.HUnit
import           Yadasm.Line
import qualified Yadasm.Symbol as S
import           Yadasm.TestContext
import qualified Yadasm.Context as C
import qualified Data.HashMap.Lazy as HashMap

tests :: [Test]
tests =
  [ TestCase
      (assertEqual
         "it should calculate total size of codewords"
         4
         (totalSize
            [defaultCodeWord { size = 1 }, defaultCodeWord { size = 3 }]))
  , TestCase
      (assertEqual
         "It should output formatted result for lines"
         (Just "correct:\nlda #$10")
         (resultToString
            testContext { C.address = 0x101
                        , C.symbols = HashMap.fromList
                            [ ( 0x101
                              , [ S.defaultSymbol { S.name = "shadowed"
                                                  , S.address = 0x101
                                                  , S.attr = S.Shadow
                                                  }
                                , S.defaultSymbol { S.name = "correct"
                                                  , S.address = 0x101
                                                  }])
                            , ( 0x100
                              , [ S.defaultSymbol { S.name = "wrong"
                                                  , S.address = 0x100
                                                  }])]
                        }
            (Just
               ( [ defaultCodeWord { text = "lda " }
                 , defaultCodeWord { text = "#$10" }]
               , []))))
  , TestCase
      (assertEqual
         "It should output formatted result for lines and respect new line"
         (Just "correct:lda \n#$10")
         (resultToString
            testContext { C.address = 0x101
                        , C.symbols = HashMap.fromList
                            [ ( 0x101
                              , [ S.defaultSymbol { S.name = "shadowed"
                                                  , S.address = 0x101
                                                  , S.attr = S.Shadow
                                                  }
                                , S.defaultSymbol { S.name = "correct"
                                                  , S.address = 0x101
                                                  , S.attr = S.Std
                                                  }])
                            , ( 0x100
                              , [ S.defaultSymbol { S.name = "wrong"
                                                  , S.address = 0x100
                                                  }])]
                        }
            (Just
               ( [ defaultCodeWord { text = "lda " }
                 , defaultCodeWord { text = "#$10", attr = NewLine }]
               , []))))
  , TestCase
      (assertEqual
         "It should output formatted result for lines with custom middle part"
         (Just "correct:\n connector! lda #$10eol!")
         (resultToString'
            (wordToString "\n")
            S.symbolToString
            " connector! "
            "eol!"
            testContext { C.address = 0x101
                        , C.symbols = HashMap.fromList
                            [ ( 0x101
                              , [ S.defaultSymbol { S.name = "correct"
                                                  , S.address = 0x101
                                                  }])
                            , ( 0x100
                              , [ S.defaultSymbol { S.name = "wrong"
                                                  , S.address = 0x100
                                                  }])]
                        }
            (Just
               ( [ defaultCodeWord { text = "lda " }
                 , defaultCodeWord { text = "#$10" }]
               , []))))
  , TestCase
      (assertEqual
         "It should output formatted result for lines and respect new lines"
         (Just "correct:\n connector! lda \n connector! #$10eol!")
         (resultToString'
            (wordToString "\n")
            S.symbolToString
            " connector! "
            "eol!"
            testContext { C.address = 0x101
                        , C.symbols = HashMap.fromList
                            [ ( 0x101
                              , [ S.defaultSymbol { S.name = "correct"
                                                  , S.address = 0x101
                                                  }])
                            , ( 0x100
                              , [ S.defaultSymbol { S.name = "wrong"
                                                  , S.address = 0x100
                                                  }])]
                        }
            (Just
               ( [ defaultCodeWord { text = "lda " }
                 , defaultCodeWord { text = "#$10", attr = NewLine }]
               , []))))
  , TestCase
      (assertEqual
         "It should return Nothing on empty input"
         Nothing
         (resultToString testContext { C.address = 0x101 } Nothing))]
