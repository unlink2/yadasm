module Main where

import qualified Context
import Numeric (showHex)

--
main :: IO ()
main = do
  putStrLn "Hello, Haskell!"
  putStrLn "Test"
  putStrLn $ showHex Context.test ""
