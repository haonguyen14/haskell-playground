module Engine (updateStorageAsyncIO) where

import CommandHandlers (Storage)
import Control (execute)
import Control.Concurrent (MVar, modifyMVar)
import Control.Concurrent.Async (Async, async)
import Control.Monad.Except (runExceptT)
import Control.Monad.State (runState)
import qualified Data.ByteString.Char8 as BS
import Resp (Value (SimpleError))

updateStoragePure :: Storage -> BS.ByteString -> (Storage, Value)
updateStoragePure s cmd =
  let (result, s') = runState (runExceptT $ execute cmd) s
   in case result of
        Right v -> (s', v)
        Left e -> (s, SimpleError e)

updateStorageIO :: MVar Storage -> BS.ByteString -> IO Value
updateStorageIO mVarStorage cmd = modifyMVar mVarStorage $ \s -> return $ updateStoragePure s cmd

updateStorageAsyncIO :: MVar Storage -> BS.ByteString -> IO (Async Value)
updateStorageAsyncIO mVarStorage cmd = async $ updateStorageIO mVarStorage cmd
