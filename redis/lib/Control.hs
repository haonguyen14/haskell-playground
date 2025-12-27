{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use lambda-case" #-}

module Control
  ( execute,
    executeCommand,
  )
where

import CommandHandlers (KVOp, command, getAllH, getH, getKV, ping, setH, setKV)
import Control.Monad.Except (throwError)
import qualified Data.ByteString.Char8 as BS
import qualified Data.HashMap.Strict as HashMap
import Resp (Value (..), bulkString, parseResp)
import Text.Parsec

commandHandlers :: HashMap.HashMap BS.ByteString ([Value] -> KVOp Value)
commandHandlers =
  HashMap.fromList
    [ ( "PING",
        \args -> case args of
          [msg] -> ping msg
          _ -> throwError "ERR wrong number of arguments for 'PING' command"
      ),
      ( "SET",
        \args -> case args of
          [key, value] -> setKV key value
          _ -> throwError "ERR wrong number of arguments for 'SET' command"
      ),
      ( "GET",
        \args -> case args of
          [key] -> getKV key
          _ -> throwError "ERR wrong number of arguments for 'GET' command"
      ),
      ( "HSET",
        \args -> case args of
          [tbl, key, value] -> setH tbl key value
          _ -> throwError "ERR wrong number of arguments for 'HSET' command"
      ),
      ( "HGET",
        \args -> case args of
          [tbl, key] -> getH tbl key
          _ -> throwError "ERR wrong number of arguments for 'HGET' command"
      ),
      ( "HGETALL",
        \args -> case args of
          [tbl] -> toResp <$> getAllH tbl
          _ -> throwError "ERR wrong number of arguments for 'HGETALL' command"
      ),
      ("COMMAND", const command)
    ]

parseInput :: BS.ByteString -> KVOp (BS.ByteString, [Value])
parseInput s = case parse parseResp "" s of
  Right (Array _ (BulkString (Just (_, cmd)) : args)) -> return (cmd, args)
  Left err -> throwError $ "ERR " ++ show err
  _ -> throwError "Expecting RESP arrays"

execute :: BS.ByteString -> KVOp Value
execute s = do
  (commandName, args) <- parseInput s
  executeCommand commandName args

executeCommand :: BS.ByteString -> [Value] -> KVOp Value
executeCommand commandName = HashMap.lookupDefault (\_ -> throwError $ "Unknown command: " ++ show commandName) commandName commandHandlers

toResp :: [(BS.ByteString, Value)] -> Value
toResp xs = Array (fromIntegral $ 2 * length xs) $ concatMap (\(k, v) -> [bulkString k, v]) xs
