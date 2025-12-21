{-# LANGUAGE OverloadedStrings #-}

module Control
  ( execute,
  )
where

import CommandHandlers (KVOp, getAllH, getH, getKV, ping, setH, setKV)
import qualified Data.ByteString.Char8 as BS
import Resp (Value (..), bulkString, parseResp)
import Text.Parsec

getCommandHandler :: BS.ByteString -> [Value] -> KVOp Value
getCommandHandler "PING" [msg] = ping msg
getCommandHandler "SET" [key, value] = setKV key value
getCommandHandler "GET" [key] = getKV key
getCommandHandler "HSET" [tbl, key, value] = setH tbl key value
getCommandHandler "HGET" [tbl, key] = getH tbl key
getCommandHandler "HGETALL" [tbl] = toResp <$> getAllH tbl
getCommandHandler cmd _ = error $ "ERR unknown command '" ++ BS.unpack cmd ++ "'"

parseInput :: BS.ByteString -> KVOp (BS.ByteString, [Value])
parseInput s = case parse parseResp "" s of
  Right (Array _ (BulkString (Just (_, cmd)) : args)) -> return (cmd, args)
  Left err -> error $ "ERR " ++ show err
  _ -> error "Expecting RESP arrays"

execute :: BS.ByteString -> KVOp Value
execute s = do
  (commandName, args) <- parseInput s
  getCommandHandler commandName args

toResp :: [(BS.ByteString, Value)] -> Value
toResp xs = Array (fromIntegral $ 2 * length xs) $ concatMap (\(k, v) -> [bulkString k, v]) xs