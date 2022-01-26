module Main where

import System.Console.ArgParser
    ( andBy,
      parsedBy,
      boolFlag,
      optFlag,
      reqPos,
      mkApp,
      runApp,
      setAppDescr,
      setAppEpilog,
      CmdLnInterface,
      ParserSpec,
      Descr(Descr) )
import qualified Yadasm.Archs.Arch6502 as A6502
import qualified Yadasm.Archs.Arch65C02 as A65C02
import qualified Yadasm.Archs.Arch65C816 as A65C816
import qualified Yadasm.Archs.Arch6502Helpers as A6502H
import qualified Yadasm.Binary as B
import qualified Data.ByteString as ByteString
import qualified Yadasm.Line as L
import qualified Yadasm.Parser as P
import qualified Yadasm.Symbol as S
import qualified Yadasm.Context as C
import qualified Yadasm.Node as N
import Data.HashMap.Lazy as HashMap ()
import           System.Console.ArgParser.BaseType (CmdLnInterface)
import           System.Console.ArgParser.Parser (andBy)
import           GHC.IO.StdHandles (openFile)
import           GHC.IO.IOMode (IOMode(ReadMode, AppendMode))
import           GHC.IO.Handle.Types (Handle)
import           System.IO (hClose)
import           Yadasm.Binary (slice)
import Yadasm ( InputData(InputData), run )

fileParser :: ParserSpec InputData
fileParser = InputData `parsedBy` reqPos "file" `Descr` "The input file"
  `andBy` optFlag "6502" "arch"
  `Descr` "The architecutre (6502, 65c02, 65c816)"
  `andBy` optFlag 0 "start"
  `Descr` "File start"
  `andBy` optFlag 0xFFFFFFFFFFFFFF "read"
  `Descr` "Read n bytes"
  `andBy` optFlag 0 "base"
  `Descr` "The start address (like .org)"
  `andBy` optFlag ":\n" "label-postfix"
  `Descr` "The label postfix (default ':')"
  `andBy` boolFlag "append"
  `Descr` "append to putput file"
  `andBy` optFlag "" "out"
  `Descr` "The output file"
  `andBy` optFlag "    " "lineindent"
  `Descr` "Line indentation"
  `andBy` optFlag "" "symindent"
  `Descr` "Symbol indentation"
  `andBy` optFlag "\n" "eol"
  `Descr` "End of line"

mainInterface :: IO (CmdLnInterface InputData)
mainInterface = (`setAppDescr` "Yadasm")
  . (`setAppEpilog` "Maintained by Lukas Krickl (lukas@krickl.dev)")
  <$> mkApp fileParser

main :: IO ()
main = do
  interface <- mainInterface
  runApp interface run

