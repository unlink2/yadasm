{-# OPTIONS_GHC -Wno-overlapping-patterns #-}

module Main where

import           System.Console.ArgParser
import           Control.Applicative
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
import           Data.HashMap.Lazy as HashMap
import           System.Console.ArgParser.BaseType (CmdLnInterface)
import           System.Console.ArgParser.Parser (andBy)
import           GHC.IO.StdHandles (openFile)
import           GHC.IO.IOMode (IOMode(ReadMode, AppendMode))
import           GHC.IO.Handle.Types (Handle)
import           System.IO (hClose)
import           Yadasm.Binary (slice)

data InputData =
  InputData { file :: String
            , arch :: String
            , fileStartOffset :: Int
            , readN :: Int
            , startAddr :: Int
            , labelPostfix :: String
            , append :: Bool
            , outfile :: String
            }
  deriving (Show)

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
  `andBy` optFlag ":" "label-postfix"
  `Descr` "The label postfix (default ':')"
  `andBy` boolFlag "append"
  `Descr` "append to putput file"
  `andBy` optFlag "" "out"
  `Descr` "The output file"

mainInterface :: IO (CmdLnInterface InputData)
mainInterface = (`setAppDescr` "Yadasm")
  . (`setAppEpilog` "Maintained by Lukas Krickl (lukas@krickl.dev)")
  <$> mkApp fileParser

main :: IO ()
main = do
  interface <- mainInterface
  runApp interface run

readBin :: FilePath -> IO ByteString.ByteString
readBin = ByteString.readFile

maybeOpenFile :: FilePath -> Bool -> Maybe (IO GHC.IO.Handle.Types.Handle)
maybeOpenFile "" append = Nothing
maybeOpenFile path False = Just $ openFile path ReadMode
maybeOpenFile path True = Just $ openFile path AppendMode

maybeCloseFile :: Maybe (IO Handle) -> IO ()
maybeCloseFile (Just handle) = do
  h <- handle
  hClose h
maybeCloseFile Nothing = return ()

outputResult (Just handle) ctx result = return ()
outputResult Nothing ctx result = print $ L.resultToString ctx result

parseUntil :: C.Context
           -> ByteString.ByteString
           -> HashMap Integer N.Node
           -> Maybe N.Node
           -> (ByteString.ByteString -> Integer)
           -> Maybe (IO GHC.IO.Handle.Types.Handle)
           -> IO ()
parseUntil ctx bin nodes defaultNode readOp handle
  | ByteString.null bin = return ()
  | otherwise = do
    outputAndLoop result
    return ()
  where
    result = P.parseAndAdvance ctx bin nodes defaultNode readOp

    outputAndLoop (result, ctx, bin) = do
      outputResult handle ctx result
      parseUntil ctx bin nodes defaultNode readOp handle
      return ()

getArch "6502" = A6502.nodes
getArch "65c02" = A65C02.nodes
getArch "65c816" = A65C816.nodes
getArch _ = []

getDefaultNode "6502" = Just A6502H.defaultNode
getDefaultNode "65c02" = Just A6502H.defaultNode
getDefaultNode "65c816" = Just A6502H.defaultNode
getDefaultNode _ = Nothing

getOpReader _ = B.read1le

run :: InputData -> IO ()
run parsed = do
  bin <- readBin (file parsed)
  let output = maybeOpenFile (outfile parsed) (append parsed)
  let ctx = C.defaultContext { C.address = toInteger $ startAddr parsed }
  let nodes = getArch (arch parsed)
  let map = P.buildLookup nodes 0xFF
  let symCtx = P.buildSymbolTable
        ctx
        bin
        map
        (getDefaultNode (arch parsed))
        (getOpReader (arch parsed))
        P.parse 
  parseUntil
    symCtx
    (B.slice
       (fileStartOffset parsed)
       (min (readN parsed) (ByteString.length bin))
       bin)
    map
    (getDefaultNode (arch parsed))
    (getOpReader (arch parsed))
    output
  maybeCloseFile output

