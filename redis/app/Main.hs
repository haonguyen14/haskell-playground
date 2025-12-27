{-# LANGUAGE OverloadedStrings #-}

module Main where

import CommandHandlers
import Control.Concurrent (forkFinally, newMVar)
import Control.Monad (forever)
import qualified Data.HashMap.Strict as HashMap
import Engine (executeCommands)
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily, addrProtocol, addrSocketType),
    AddrInfoFlag (AI_PASSIVE),
    PortNumber,
    SocketOption (NoDelay, ReuseAddr),
    accept,
    addrFlags,
    bind,
    close,
    defaultHints,
    getAddrInfo,
    listen,
    setSocketOption,
    socket,
  )

initStorage :: Storage
initStorage = Storage {kvStore = KVStore HashMap.empty, hStore = KVStore HashMap.empty}

portNumber :: PortNumber
portNumber = 6379

main :: IO ()
main = do
  putStrLn "Starting Server..."
  addrInfo <-
    getAddrInfo
      (Just (defaultHints {addrFlags = [AI_PASSIVE]}))
      Nothing
      (Just . show $ portNumber)

  let serverAddrInfo = head addrInfo

  sock <- socket (addrFamily serverAddrInfo) (addrSocketType serverAddrInfo) (addrProtocol serverAddrInfo)
  setSocketOption sock ReuseAddr 1
  setSocketOption sock NoDelay 1

  bind sock (addrAddress serverAddrInfo)
  listen sock 2

  putStrLn "Server is ready"

  -- initialize a new in-memory storage
  kvStorage <- newMVar initStorage

  forever $ do
    (conn, clientAddr) <- accept sock
    putStrLn $ "Accepted connection from " ++ show clientAddr
    forkFinally (executeCommands conn kvStorage) (\_ -> putStrLn "Connection Closed" >> close conn)
