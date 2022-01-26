module Yadasm where

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
import           GHC.IO.StdHandles (openFile, stdout, openBinaryFile)
import           GHC.IO.IOMode (IOMode(ReadMode, AppendMode, WriteMode))
import           GHC.IO.Handle.Types (Handle)
import           System.IO (hClose, hPutStr)
import           Yadasm.Binary (slice)
import           System.Directory (doesFileExist)

data InputData =
  InputData { file :: String
            , arch :: String
            , fileStartOffset :: Int
            , readN :: Int
            , startAddr :: Int
            , symbolPostfix :: String
            , append :: Bool
            , outfile :: String
            , lineIndent :: String
            , symbolIndent :: String
            , eol :: String
            }
  deriving (Show)

readBin :: FilePath -> IO ByteString.ByteString
readBin = ByteString.readFile

maybeOpenFile :: FilePath -> Bool -> IO Handle
maybeOpenFile "" append = do
  return stdout
maybeOpenFile path False = openFile path WriteMode
maybeOpenFile path True = openFile path AppendMode

maybeCreateFile "" = do
  return ()
maybeCreateFile path = do
  exists <- doesFileExist path
  maybeCreateFile' path exists

maybeCreateFile' path False = writeFile path ""
maybeCreateFile' path True = do
  return ()

maybeCloseFile :: Handle -> String -> IO ()
maybeCloseFile handle "" = return ()
maybeCloseFile handle path = hClose handle

toString :: Maybe String -> String
toString (Just s) = s
toString Nothing = ""

outputResult :: String
             -> String
             -> String
             -> Handle
             -> C.Context
             -> Maybe ([L.CodeWord], [S.Symbol])
             -> IO ()
outputResult lineIndent symIndent symPost handle ctx result = do
  hPutStr
    handle
    (toString
       (L.resultToString'
          L.wordToString
          (S.symbolToString' symIndent symPost)
          lineIndent
          "\n"
          ctx
          result))

parseUntil
  :: InputData
  -> C.Context
  -> ByteString.ByteString
  -> HashMap Integer N.Node
  -> Maybe N.Node
  -> (ByteString.ByteString -> Integer)
  -> Handle
  -> (C.Context
      -> ByteString.ByteString
      -> HashMap Integer N.Node
      -> Maybe N.Node
      -> (ByteString.ByteString -> Integer)
      -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString))
  -> (String
      -> String
      -> String
      -> Handle
      -> C.Context
      -> Maybe ([L.CodeWord], [S.Symbol])
      -> IO ())
  -> IO ()
parseUntil input ctx bin nodes defaultNode readOp handle parse outputResult
  | ByteString.null bin = return ()
  | otherwise = do
    outputAndLoop result
    return ()
  where
    result = parse ctx bin nodes defaultNode readOp

    outputAndLoop (result, ctx, bin) = do
      outputResult
        (lineIndent input)
        (symbolIndent input)
        (symbolPostfix input)
        handle
        ctx
        result
      parseUntil
        input
        (P.advanceCtx ctx result)
        (P.advanceBin bin result)
        nodes
        defaultNode
        readOp
        handle
        parse
        outputResult
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

run'
  :: (C.Context
      -> ByteString.ByteString
      -> HashMap Integer N.Node
      -> Maybe N.Node
      -> (ByteString.ByteString -> Integer)
      -> (Maybe ([L.CodeWord], [S.Symbol]), C.Context, ByteString.ByteString))
  -> InputData
  -> IO ()
run' parse parsed = do
  maybeCreateFile (outfile parsed)
  bin <- readBin (file parsed)
  output <- maybeOpenFile (outfile parsed) (append parsed)
  let ctx = C.defaultContext { C.address = toInteger $ startAddr parsed }
  let nodes = getArch (arch parsed)
  let map = P.buildLookup nodes 0xFF
  let symCtx = P.buildSymbolTable
        ctx
        bin
        map
        (getDefaultNode (arch parsed))
        (getOpReader (arch parsed))
        parse
  parseUntil
    parsed
    symCtx
    (B.slice
       (fileStartOffset parsed)
       (min (readN parsed) (ByteString.length bin))
       bin)
    map
    (getDefaultNode (arch parsed))
    (getOpReader (arch parsed))
    output
    parse
    outputResult
  maybeCloseFile output (outfile parsed)

run :: InputData -> IO ()
run = run' P.parse
