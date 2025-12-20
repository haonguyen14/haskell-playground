{-# LANGUAGE OverloadedStrings #-}

module Main where

import CommandHandlers
import Control
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import Resp

executeMultipleCommands :: Storage -> [BS.ByteString] -> [Value]
executeMultipleCommands _ [] = []
executeMultipleCommands s (x : xs) = case execute x of
  Right m -> let (output, s') = runState m s in output : executeMultipleCommands s' xs
  Left err -> SimpleError err : executeMultipleCommands s xs

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
      initState =
        Storage
          { kvStore = KVStore HashMap.empty,
            hStore = KVStore HashMap.empty
          }
      results = executeMultipleCommands initState (map BS.pack commands)
  mapM_ print results
