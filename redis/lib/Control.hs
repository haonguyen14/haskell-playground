{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Control
  ( execute,
  )
where

import CommandHandlers (KVOp, getAllH, getH, getKV, ping, setH, setKV)
import qualified Data.ByteString.Char8 as BS
import Resp (Value (..), bulkString, parseResp)
import Text.Parsec

data Command a where
  Ping :: Value -> Command (KVOp Value)
  Set :: Value -> Value -> Command (KVOp Value)
  Get :: Value -> Command (KVOp Value)
  SetH :: Value -> Value -> Value -> Command (KVOp Value)
  GetH :: Value -> Value -> Command (KVOp Value)
  GetAllH :: Value -> Command (KVOp [(BS.ByteString, Value)])

data SomeCommand = forall a. SomeCommand (Command a)

getCommandHandler :: BS.ByteString -> [Value] -> KVOp SomeCommand
getCommandHandler "PING" [msg] = return $ SomeCommand (Ping msg)
getCommandHandler "SET" [key, value] = return $ SomeCommand (Set key value)
getCommandHandler "GET" [key] = return $ SomeCommand (Get key)
getCommandHandler "HSET" [tbl, key, value] = return $ SomeCommand (SetH tbl key value)
getCommandHandler "HGET" [tbl, key] = return $ SomeCommand (GetH tbl key)
getCommandHandler "HGETALL" [tbl] = return $ SomeCommand (GetAllH tbl)
getCommandHandler cmd _ = error $ "ERR unknown command '" ++ BS.unpack cmd ++ "'"

executeCommand :: SomeCommand -> KVOp Value
executeCommand (SomeCommand cmd) = case cmd of
  Ping msg -> ping msg
  Set key value -> setKV key value
  Get key -> getKV key
  SetH tbl key value -> setH tbl key value
  GetH tbl key -> getH tbl key
  GetAllH tbl -> toResp <$> getAllH tbl

parseInput :: BS.ByteString -> KVOp (BS.ByteString, [Value])
parseInput s = case parse parseResp "" s of
  Right (Array _ (BulkString (Just (_, cmd)) : args)) -> return (cmd, args)
  Left err -> error $ "ERR " ++ show err
  _ -> error "Expecting RESP arrays"

execute :: BS.ByteString -> KVOp Value
execute s = do
  (commandName, args) <- parseInput s
  handler <- getCommandHandler commandName args
  executeCommand handler

toResp :: [(BS.ByteString, Value)] -> Value
toResp xs = Array (fromIntegral $ 2 * length xs) $ concatMap (\(k, v) -> [bulkString k, v]) xs