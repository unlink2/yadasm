module Yadasm.Binary where

import qualified Data.ByteString as B
import           Data.Word (Word8)
import           Data.Maybe (isNothing)

sliceEnd :: Int -> B.ByteString -> B.ByteString
sliceEnd = slice 0

slice :: Int -> Int -> B.ByteString -> B.ByteString
slice start count bin = B.take count $ B.drop start bin

toInteger :: B.ByteString -> Integer
toInteger = B.foldl toIntegerOp 0

toIntegerOp :: Integer -> Word8 -> Integer
toIntegerOp prev v = (prev * 256) + Prelude.toInteger v

readNle :: Int -> B.ByteString -> Integer
readNle x bin = Yadasm.Binary.toInteger (B.reverse (sliceEnd x bin))

read1le :: B.ByteString -> Integer
read1le = readNle 1

read2le :: B.ByteString -> Integer
read2le = readNle 2
