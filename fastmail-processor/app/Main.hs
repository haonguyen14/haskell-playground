{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Brick
import Brick.BChan (BChan, newBChan, writeBChan)
import Brick.Widgets.Border (border, borderWithLabel)
import Brick.Widgets.Center (center, centerLayer)
import Brick.Widgets.List (List, handleListEvent, list, listElements, listModify, listSelectedAttr, listSelectedElement, renderList)
import Control.Concurrent (forkIO)
import Control.Monad.IO.Class (MonadIO (liftIO))
import Control.Monad.Trans.Except (ExceptT, runExceptT, throwE)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as C8
import qualified Data.Text as T
import qualified Data.Vector as V
import Fastmail (Email (..), EmailAddress (..), archiveEmail, getMailboxIds, getSession)
import Graphics.Vty (Event (EvKey), Key (KChar, KDel, KDown, KEnter, KEsc, KUp))
import qualified Graphics.Vty as Vty
import qualified Graphics.Vty.CrossPlatform as VCP
import Lib (getEmailsByPage)
import System.Environment (lookupEnv)

data CustomEvent
  = PageLoaded (Either String [Email]) Integer
  | EmailArchived (Either String [String])

data Name = EmailList deriving (Ord, Show, Eq)

data AppState = AppState
  { _emailList :: List Name (Email, Bool),
    _focusedEmail :: Maybe Email,
    _offset :: Integer,
    _limit :: Integer,
    _archiveId :: String,
    _apiToken :: BS.ByteString,
    _loading :: Maybe String,
    _chan :: BChan CustomEvent
  }

drawUI :: AppState -> [Widget Name]
drawUI s = [loadingLayer, popup, mainLayer]
  where
    elements = listElements (_emailList s)

    -- loading screen
    loadingLayer = case _loading s of
      Just t -> centerLayer $ border $ padAll 1 $ str t
      Nothing -> emptyWidget

    -- mainLayer construction
    label = str $ " Fastmail Inbox (Emails " ++ show (_offset s) ++ "-" ++ show (_offset s + _limit s) ++ ") "
    mainLayer =
      if V.null elements
        then
          center $
            borderWithLabel (str " Notice ") $
              padAll 2 $
                vBox
                  [ str " This page is empty! ",
                    str " All emails archived or none found ",
                    str " ",
                    str " [n] Next Page | [p] Previous Page"
                  ]
        else center $ borderWithLabel label $ renderList drawEmail True (_emailList s)

    -- popup construction
    popup = case (_focusedEmail s) of
      Just e ->
        centerLayer $
          borderWithLabel (str $ " Subject: " ++ T.unpack (subject e)) $
            padAll 1 $
              strWrap (truncateBody (body e) 2000)
      Nothing -> emptyWidget

truncateBody :: T.Text -> Int -> String
truncateBody t maxLen =
  if T.length t > maxLen
    then T.unpack (T.take maxLen t) ++ "\n\n... [Content Truncated]"
    else T.unpack t

drawEmail :: Bool -> (Email, Bool) -> Widget Name
drawEmail isCursor (e, selected) =
  let subj = T.unpack $ subject e
      prefix = if selected then "[x] " else "[ ] "
      senderStr = case from e of
        (EmailAddress mName emailAddr : _) ->
          let n = maybe "" T.unpack mName
           in n ++ " [" ++ T.unpack emailAddr ++ "]"
        _ -> "Unknown"

      subjWidget = padRight Max $ str (prefix <> subj)
      senderWidget = padLeft (Pad 2) $ str senderStr

      attr = if isCursor then withAttr listSelectedAttr else id
   in attr $ hBox [subjWidget, senderWidget]

handlePopupEvent :: BrickEvent Name CustomEvent -> AppState -> EventM Name AppState ()
handlePopupEvent (VtyEvent (EvKey (KChar 'q') [])) s = put s {_focusedEmail = Nothing}
handlePopupEvent _ _ = return ()

handleListModeEvent :: BrickEvent Name CustomEvent -> AppState -> EventM Name AppState ()
handleListModeEvent (VtyEvent (EvKey KEsc [])) _ = halt
handleListModeEvent (VtyEvent (EvKey KDel [])) s = archiveBulkEmails s nonSelectedIds
  where
    elements = listElements (_emailList s)
    nonSelectedIds = [emailId e | (e, False) <- V.toList elements]
handleListModeEvent (VtyEvent (EvKey KEnter [])) s = case listSelectedElement (_emailList s) of
  Just (_, (e, _)) -> put s {_focusedEmail = Just e}
  Nothing -> return ()
handleListModeEvent (VtyEvent (EvKey (KChar 'n') [])) s = fetchNewPage s (_offset s + _limit s)
handleListModeEvent (VtyEvent (EvKey (KChar 'p') [])) s = fetchNewPage s (max 0 (_offset s - _limit s))
handleListModeEvent (VtyEvent (EvKey (KChar ' ') [])) s = put s {_emailList = newList}
  where
    newList = listModify (\(e, sel) -> (e, not sel)) (_emailList s)
handleListModeEvent (VtyEvent e) _ = handleListEventVi e
handleListModeEvent _ _ = return ()

appEvent :: BrickEvent Name CustomEvent -> EventM Name AppState ()
appEvent (AppEvent (PageLoaded result newOffset)) = do
  s <- get
  case result of
    Right newEmails ->
      put
        s
          { _emailList = list EmailList (V.fromList (map (\e -> (e, False)) newEmails)) 1,
            _loading = Nothing,
            _offset = newOffset
          }
    Left _ -> put s {_loading = Nothing}
appEvent (AppEvent (EmailArchived result)) = do
  s <- get
  case result of
    Right archivedIds -> do
      let currentList = _emailList s
          -- Keep only emails that were NOT archived
          newVec = V.filter (\(e, _) -> emailId e `notElem` archivedIds) (listElements currentList)
          newList = list EmailList newVec 1
      put s {_emailList = newList, _loading = Nothing}
    Left _ -> put s {_loading = Nothing}
appEvent e = do
  s <- get
  case (_focusedEmail s) of
    Just _ -> handlePopupEvent e s
    Nothing -> handleListModeEvent e s

-- A small helper to handle list events and Vim keys
handleListEventVi :: Vty.Event -> EventM Name AppState ()
handleListEventVi e = do
  s <- get
  let vtyEvent = case e of
        EvKey (KChar 'j') [] -> EvKey KDown []
        EvKey (KChar 'k') [] -> EvKey KUp []
        _ -> e
  res <- nestEventM (_emailList s) (handleListEvent vtyEvent)
  put s {_emailList = fst res}

archiveBulkEmails :: AppState -> [String] -> EventM Name AppState ()
archiveBulkEmails _ [] = return ()
archiveBulkEmails s ids = do
  put s {_loading = Just "Archiving..."}
  let chan = _chan s
      token = _apiToken s
      aId = _archiveId s

  _ <- liftIO $ forkIO $ do
    res <- runExceptT $ do
      session <- getSession token
      archiveEmail session token aId ids
    case res of
      Right () -> writeBChan chan (EmailArchived (Right ids))
      Left err -> writeBChan chan (EmailArchived (Left err))

  return ()

fetchNewPage :: AppState -> Integer -> EventM Name AppState ()
fetchNewPage s newOffset = do
  put s {_loading = Just "Fetching emails..."}

  let chan = _chan s
      token = _apiToken s
      lim = _limit s

  _ <- liftIO $ forkIO $ do
    res <- runExceptT $ do
      session <- getSession token
      getEmailsByPage (newOffset, lim) token session
    writeBChan chan (PageLoaded res newOffset)

  return ()

-- 4. Define the App structure
app :: App AppState CustomEvent Name
app =
  App
    { appDraw = drawUI,
      appChooseCursor = showFirstCursor,
      appHandleEvent = appEvent,
      appStartEvent = return (),
      appAttrMap = const $ attrMap Vty.defAttr [(listSelectedAttr, Vty.black `Brick.on` Vty.white)]
    }

-- 5. Integration with your Fastmail logic
getApiToken :: ExceptT String IO BS.ByteString
getApiToken = do
  mToken <- liftIO $ lookupEnv "FASTMAIL_TOKEN"
  case mToken of
    Nothing -> throwE "Cannot get fastmail token"
    Just token -> return $ C8.pack token

runTUI :: ExceptT String IO ()
runTUI = do
  apiToken <- getApiToken
  session <- getSession apiToken

  let initialOffset = 0
  let initialLimit = 100

  chan <- liftIO $ newBChan 10
  emails <- getEmailsByPage (initialOffset, initialLimit) apiToken session
  archiveIds <- getMailboxIds session apiToken "Archive"

  case archiveIds of
    (archiveId : _) -> do
      let initialState =
            AppState
              { _emailList = list EmailList (V.fromList $ map (\e -> (e, False)) emails) 1,
                _archiveId = archiveId,
                _focusedEmail = Nothing,
                _offset = initialOffset,
                _limit = initialLimit,
                _apiToken = apiToken,
                _loading = Nothing,
                _chan = chan
              }
          buildVty = VCP.mkVty Vty.defaultConfig
      initialVty <- liftIO buildVty
      _ <- liftIO $ customMain initialVty buildVty (Just chan) app initialState
      return ()
    _ -> throwE "Cannot get Archive mailbox id"

main :: IO ()
main = do
  result <- runExceptT runTUI
  case result of
    Left err -> putStrLn $ "Error: " ++ err
    Right _ -> putStrLn "Goodbye!"
