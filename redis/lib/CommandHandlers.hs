{-# LANGUAGE NamedFieldPuns #-}

module CommandHandlers (
  KVStore (..),
  Storage (..),
  KVOp,
  setKV,
  getKV,
  setH,
  getH,
  getAllH,
  ping,
)
where

import Control.Monad.Except (ExceptT)
import Control.Monad.State
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import Resp (Value (..))

newtype KVStore a = KVStore (HashMap.HashMap BS.ByteString a) deriving (Show)

data Storage = Storage
  { kvStore :: KVStore Value
  , hStore :: KVStore (KVStore Value)
  }
  deriving (Show)

type KVOp a = ExceptT String (State Storage) a

ping :: Value -> KVOp Value
ping (SimpleString msg) = return $ SimpleString msg
ping _ = error "Err invalid PING command"

setKV :: Value -> Value -> KVOp Value
setKV (BulkString (Just (_, key))) value = do
  Storage{kvStore, hStore} <- get
  let KVStore kv = kvStore
   in put $ Storage{kvStore = KVStore (HashMap.insert key value kv), hStore}
  return $ SimpleString "OK"
setKV _ _ = error "Err invalid key for SET command"

getKV :: Value -> KVOp Value
getKV v
  | BulkString (Just (_, key)) <- v = do
      Storage{kvStore = KVStore kv} <- get
      case HashMap.lookup key kv of
        Just value -> return value
        _ -> return Null
  | otherwise = error "Err invalid key for GET commadn"

setH :: Value -> Value -> Value -> KVOp Value
setH tbl key value
  | (BulkString (Just (_, idx))) <- tbl
  , (BulkString (Just (_, k))) <- key = do
      Storage{kvStore, hStore = KVStore kv} <- get
      let KVStore hm = HashMap.lookupDefault (KVStore HashMap.empty) idx kv
          kv' = HashMap.insert idx (KVStore (HashMap.insert k value hm)) kv
       in put $ Storage{kvStore, hStore = KVStore kv'}
      return $ SimpleString "OK"
  | otherwise = error "Err invalid key for HSET command"

getH :: Value -> Value -> KVOp Value
getH tbl key
  | (BulkString (Just (_, idx))) <- tbl
  , (BulkString (Just (_, k))) <- key = do
      Storage{hStore = KVStore kv} <- get
      let KVStore hm = HashMap.lookupDefault (KVStore HashMap.empty) idx kv
      case HashMap.lookup k hm of
        Just v -> return v
        _ -> return Null
  | otherwise = error "Err invalid key for HGET command"

getAllH :: Value -> KVOp [(BS.ByteString, Value)]
getAllH tbl
  | BulkString (Just (_, idx)) <- tbl = do
      Storage{hStore = KVStore kv} <- get
      case HashMap.lookup idx kv of
        Just (KVStore hm) -> return $ HashMap.toList hm
        _ -> return []
  | otherwise = error "Err invalid key for HGETALL command"
