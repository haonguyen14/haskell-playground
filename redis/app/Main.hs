{-# LANGUAGE OverloadedStrings #-}

module Main where

import CommandHandlers
import Control
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import Data.Traversable (mapAccumL)
import Data.Tuple (swap)
import Resp

initStorage :: Storage
initStorage = Storage {kvStore = KVStore HashMap.empty, hStore = KVStore HashMap.empty}

executeMultipleCommands :: [BS.ByteString] -> [Value]
executeMultipleCommands xs = snd $ mapAccumL accF initStorage xs
  where
    accF s bs = swap $ case execute bs of
      Right m -> runState m s
      Left e -> (SimpleError e, s)

main :: IO ()
main = do
  let commands =
        [ "*2\r\n$4\r\nPING\r\n+Hello\r\n",
          "*3\r\n$3\r\nSET\r\n$3\r\nkey\r\n$5\r\nvalue\r\n",
          "*2\r\n$3\r\nGET\r\n$3\r\nkey\r\n",
          "*2\r\n$3\r\nGET\r\n$7\r\nunknown\r\n",
          "*4\r\n$4\r\nHSET\r\n$5\r\ntable\r\n$3\r\nkey\r\n$5\r\nvalue\r\n",
          "*4\r\n$4\r\nHSET\r\n$5\r\ntable\r\n$4\r\nkey2\r\n$5\r\nvalue\r\n",
          "*3\r\n$4\r\nHGET\r\n$5\r\ntable\r\n$3\r\nkey\r\n",
          "*3\r\n$4\r\nHGET\r\n$5\r\ntable\r\n$7\r\nunknown\r\n",
          "*2\r\n$7\r\nHGETALL\r\n$5\r\ntable\r\n",
          "*2\r\n$7\r\nHGETALL\r\n$7\r\nunknown\r\n"
        ]
      results = executeMultipleCommands (map BS.pack commands)
  mapM_ print results
