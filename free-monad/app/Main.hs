{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad.Free
import qualified Data.ByteString as BS
import Lib (Ops, executeIO, executePrint, readFileM, writeFileM)

program :: Free Ops BS.ByteString
program = do
  writeFileM filePath "test data"
  readFileM filePath
  where
    filePath = "output.txt"

main :: IO ()
main = do
  result <- executeIO program
  print result
