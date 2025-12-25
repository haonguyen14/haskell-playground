{-# LANGUAGE OverloadedStrings #-}

module Main where

import CommandHandlers
import Control
import Control.Concurrent (newMVar, takeMVar)
import Control.Concurrent.Async (wait)
import Control.Monad.Except (runExceptT)
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import Data.Foldable (forM_)
import qualified Data.HashMap.Strict as HashMap
import Data.Traversable (mapAccumL)
import Engine (updateStorageAsyncIO)
import Resp

initStorage :: Storage
initStorage = Storage {kvStore = KVStore HashMap.empty, hStore = KVStore HashMap.empty}

executeMultipleCommands :: [BS.ByteString] -> [Value]
executeMultipleCommands xs = snd $ mapAccumL accF initStorage xs
  where
    accF s bs =
      let (res, s') = runState (runExceptT $ execute bs) s
       in (s', either SimpleError id res)

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

  storage <- newMVar initStorage

  asyncOps <- mapM (updateStorageAsyncIO storage . BS.pack) commands

  forM_ asyncOps $ \async -> do
    result <- wait async
    print result

  finalStorage <- takeMVar storage

  putStrLn "----- Final Storage -----"
  print finalStorage
