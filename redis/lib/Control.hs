{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}

module Control
  ( execute,
  )
where

import CommandHandlers (KVDatabase, getKV, ping, setKV, setH, getH, getAllH)
import qualified Data.ByteString.Char8 as BS
import Resp (ToResp, toResp, Value (..), parseResp)
import Text.Parsec

data Command a where
  Ping :: Value -> Command Value
  Set :: Value -> Value -> Command Value
  Get :: Value -> Command (Maybe Value)
  SetH :: Value -> Value -> Value -> Command Value
  GetH :: Value -> Value -> Command (Maybe Value)
  GetAllH :: Value -> Command (Either Value (Maybe [(BS.ByteString, Value)]))

data SomeCommand = forall a. (ToResp a) => SomeCommand (Command a)

getCommandHandler :: BS.ByteString -> [Value] -> Either String SomeCommand
getCommandHandler "PING" [msg] = Right $ SomeCommand (Ping msg)
getCommandHandler "SET" [key, value] = Right $ SomeCommand (Set key value)
getCommandHandler "GET" [key] = Right $ SomeCommand (Get key)
getCommandHandler "HSET" [tbl, key, value] = Right $ SomeCommand (SetH tbl key value)
getCommandHandler "HGET" [tbl, key] = Right $ SomeCommand (GetH tbl key)
getCommandHandler "HGETALL" [tbl] = Right $ SomeCommand (GetAllH tbl)
getCommandHandler cmd _ = Left $ "ERR unknown command '" ++ BS.unpack cmd ++ "'"

executeCommand' :: Command a -> KVDatabase a
executeCommand' (Ping msg) = ping  msg
executeCommand' (Set key value) = setKV key value
executeCommand' (Get key) = getKV key
executeCommand' (SetH tbl key value) = setH tbl key value
executeCommand' (GetH tbl key) = getH tbl key
executeCommand' (GetAllH tbl) = getAllH tbl

executeCommand :: SomeCommand -> KVDatabase Value
executeCommand (SomeCommand cmd) = toResp <$> executeCommand' cmd

parseInput :: BS.ByteString -> Either String (BS.ByteString, [Value])
parseInput s = case parse parseResp "" s of
  Right (Array _ (BulkString (Just (_, cmd)) : args)) -> Right (cmd, args)
  Left err -> Left $ show err
  _ -> Left "Expecting RESP arrays"

execute :: BS.ByteString -> Either String (KVDatabase Value)
execute s = do
  (commandName, args) <- parseInput s
  handler <- getCommandHandler commandName args
  return $ executeCommand handler
