{-# LANGUAGE FlexibleInstances #-}

module Resp
  ( Value (..),
    parseResp,
    bulkString,
  )
where

import qualified Data.ByteString.Char8 as BS
import Text.Parsec

data Value
  = SimpleString String
  | BulkString (Maybe (Integer, BS.ByteString))
  | SimpleError String
  | Integer Integer
  | Boolean Bool
  | Null
  | Array Integer [Value]
  deriving (Show)

parseResp :: Parsec BS.ByteString () Value
parseResp =
  parseSimpleString
    <|> parseBulkString
    <|> parseSimpleError
    <|> parseInteger
    <|> parseBoolean
    <|> parseNull
    <|> parseArray

rn :: Parsec BS.ByteString () String
rn = string "\r\n"

parseSimpleString :: Parsec BS.ByteString () Value
parseSimpleString = char '+' >> manyTill anyChar (try rn) >>= \str -> return $ SimpleString str

parseBulkString :: Parsec BS.ByteString () Value
parseBulkString = do
  _ <- char '$'
  len <- string "-1" <|> many1 digit
  _ <- rn
  if len == "-1"
    then return $ BulkString Nothing
    else do
      let l = read len :: Integer
      str <- count (fromIntegral l) anyChar
      _ <- rn
      return $ BulkString (Just (l, BS.pack str))

parseSimpleError :: Parsec BS.ByteString () Value
parseSimpleError = char '-' >> manyTill anyChar (try rn) >>= \err -> return $ SimpleError err

parseInteger :: Parsec BS.ByteString () Value
parseInteger =
  char ':' >> do
    sign <- optionMaybe (oneOf "+-")
    numStr <- many1 digit
    _ <- rn
    case sign of
      Just '-' -> return $ Integer (negate (read numStr))
      _ -> return $ Integer (read numStr)

parseBoolean :: Parsec BS.ByteString () Value
parseBoolean = do
  _ <- char '#'
  b <- oneOf "tf"
  _ <- rn
  return $ Boolean (b == 't')

parseNull :: Parsec BS.ByteString () Value
parseNull = string "_\r\n" >> return Null

parseArray :: Parsec BS.ByteString () Value
parseArray = do
  _ <- char '*'
  len <- manyTill digit rn
  let l = read len :: Integer
  elements <- count (fromIntegral l) parseResp
  return $ Array l elements

bulkString :: BS.ByteString -> Value
bulkString a = BulkString $ Just (l, a)
  where
    l = fromIntegral $ BS.length a