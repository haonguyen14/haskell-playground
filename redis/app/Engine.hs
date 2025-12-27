module Engine (updateStorageAsyncIO, executeCommands) where

import CommandHandlers (Storage)
import Control (execute, executeCommand)
import Control.Concurrent (MVar, modifyMVar)
import Control.Concurrent.Async (Async, async)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import qualified Data.ByteString.Char8 as BS
import Network.Socket (Socket)
import Network.Socket.ByteString (recv, sendAll)
import Resp (Value (Array, BulkString, SimpleError), parseResp, toByteString)
import Text.Parsec (ParseError, parse)
import Text.Parsec.Error (Message (SysUnExpect), errorMessages)

updateStoragePure :: Storage -> BS.ByteString -> (Storage, Value)
updateStoragePure s cmd =
  let (result, s') = runState (runExceptT $ execute cmd) s
   in case result of
        Right v -> (s', v)
        Left e -> (s, Resp.SimpleError e)

updateStorageIO :: MVar Storage -> BS.ByteString -> IO Value
updateStorageIO mVarStorage cmd = modifyMVar mVarStorage $ \s -> return $ updateStoragePure s cmd

updateStorageAsyncIO :: MVar Storage -> BS.ByteString -> IO (Async Value)
updateStorageAsyncIO mVarStorage cmd = async $ updateStorageIO mVarStorage cmd

isMoreDataNeeded :: ParseError -> Bool
isMoreDataNeeded e = any isMoreDataNeeded' (errorMessages e)
  where
    isMoreDataNeeded' (SysUnExpect "") = True
    isMoreDataNeeded' _ = False

executeCommands :: Socket -> MVar Storage -> IO ()
executeCommands sock storage = do
  x <- readCommand sock BS.empty
  case x of
    Right (cmd, args) -> do
      output <- modifyMVar storage $ \s ->
        let (result, s') = runState (runExceptT $ executeCommand cmd args) s
         in case result of
              Right v -> return (s', v)
              Left e -> return (s, SimpleError e)
      sendAll sock (toByteString output)
      executeCommands sock storage
    Left e -> print e

readCommand :: Socket -> BS.ByteString -> IO (Either String (BS.ByteString, [Value]))
readCommand sock current = do
  let parseResult = parse parseResp "" current

  case parseResult of
    Right v -> do
      return . Right $ toNameAndArgs v
    Left err | isMoreDataNeeded err -> do
      next <- recv sock 1024
      if BS.null next
        then return $ Left "Connnection is already closed"
        else readCommand sock (current `BS.append` next)
    Left err -> return . Left $ "Failed with error: " ++ show err
  where
    toNameAndArgs (Array _ (BulkString (Just (_, cmd)) : args)) = (cmd, args)
    toNameAndArgs _ = error "Unexpected error. This can be fixed easily"
