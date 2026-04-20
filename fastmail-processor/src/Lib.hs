{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( getEmailsByPage,
  )
where

import Control.Monad.Trans.Except (ExceptT, throwE)
import qualified Data.ByteString as BS
import Fastmail

getEmailsByPage :: Page -> BS.ByteString -> Session -> ExceptT String IO [Email]
getEmailsByPage page apiToken session = do
  eIds <-
    (getMailboxIds session apiToken "Inbox") >>= \mailbox -> case mailbox of
      (mailboxId : _) -> retrieveMailbox page session apiToken mailboxId
      _ -> throwE "Cannot find mailbox ID"
  getEmails session apiToken eIds
