{-# LANGUAGE NamedFieldPuns #-}

module CommandHandlers
  ( KVStore (..),
    Storage (..),
    KVDatabase,
    setKV,
    getKV,
    setH,
    getH,
    getAllH,
    ping,
  )
where

import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import Resp (Value (..))

newtype KVStore a = KVStore (HashMap.HashMap BS.ByteString a) deriving (Show)

data Storage = Storage
  { kvStore :: KVStore Value,
    hStore :: KVStore (KVStore Value)
  }
  deriving (Show)

type KVDatabase a = State Storage a

ping :: Value -> KVDatabase Value
ping (SimpleString msg) = return $ SimpleString msg
ping _ = return $ SimpleError "ERR PING message must be a Simple String"

setKV :: Value -> Value -> KVDatabase Value
setKV (BulkString (Just (_, key))) value = do
  Storage {kvStore, hStore} <- get
  let KVStore kv = kvStore
   in put $ Storage {kvStore = KVStore (HashMap.insert key value kv), hStore}
  return $ SimpleString "OK"
setKV _ _ = return . SimpleError $ "Err invalid key for SET command"

getKV :: Value -> KVDatabase (Maybe Value)
getKV v
  | BulkString (Just (_, key)) <- v = do
      Storage {kvStore} <- get
      let KVStore kv = kvStore in return $ HashMap.lookup key kv
  | otherwise = return . Just . SimpleError $ "Err invalid key for GET commadn"

setH :: Value -> Value -> Value -> KVDatabase Value
setH tbl key value
  | (BulkString (Just (_, idx))) <- tbl,
    (BulkString (Just (_, k))) <- key = do
      Storage {kvStore, hStore = KVStore kv} <- get
      let KVStore hm = HashMap.lookupDefault (KVStore HashMap.empty) idx kv
          kv' = HashMap.insert idx (KVStore (HashMap.insert k value hm)) kv
       in put $ Storage {kvStore, hStore = KVStore kv'}
      return $ SimpleString "OK"
  | otherwise = return . SimpleError $ "Err invalid key for HSET command"

getH :: Value -> Value -> KVDatabase (Maybe Value)
getH tbl key
  | (BulkString (Just (_, idx))) <- tbl,
    (BulkString (Just (_, k))) <- key = do
      Storage {hStore = KVStore kv} <- get
      let KVStore hm = HashMap.lookupDefault (KVStore HashMap.empty) idx kv
       in return $ HashMap.lookup k hm
  | otherwise = return . Just . SimpleError $ "Err invalid key for HGET command"

getAllH :: Value -> KVDatabase (Either Value (Maybe [(BS.ByteString, Value)]))
getAllH tbl
  | BulkString (Just (_, idx)) <- tbl = do
      Storage { hStore = KVStore kv } <- get
      case HashMap.lookup idx kv of
        Just (KVStore hm) -> return . Right . Just $ HashMap.toList hm
        _ -> return . Right $ Nothing
  | otherwise = return . Left . SimpleError $ "Err invalid key for HGETALL command"