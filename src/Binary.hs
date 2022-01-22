module Binary where

import qualified Data.ByteString as B
import           Data.Word (Word8)

class InputFile a where
  read :: a -> Int -> B.ByteString
  next :: a -> B.ByteString
  getEnd :: a -> Maybe Int -> Int
  isAtEnd :: a -> Bool
  length :: a -> Int
  isEmpty :: a -> Bool
  rewind :: a -> Int -> a
  reset :: a -> Binary
  advance :: a -> Int -> a

data Binary = Binary { bytes :: B.ByteString
                     , offset :: Int
                     , start :: Int
                     , end :: Maybe Int
                     }

instance InputFile Binary where
  read bin count = B.take count (B.drop (offset bin) (bytes bin))

  next bin = B.take (offset bin) (bytes bin)

  getEnd bin (Just end) = end
  getEnd bin Nothing = Binary.length bin

  isAtEnd bin = offset bin > getEnd bin (end bin)

  length bin = start bin - getEnd bin (end bin)

  isEmpty bin = B.null (bytes bin)

  rewind bin count = bin { offset = offset bin - count }

  reset bin = bin { offset = start bin }

  advance bin count = bin { offset = offset bin + count }
