{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module MyLib
  ( JValue (..),
    extractDigits,
    parse,
  )
where

import Data.Char (isDigit)
import Data.List (isPrefixOf)
import Data.Map (Map, empty, insert)

data JValue
  = JObject (Map String JValue)
  | JArray [JValue]
  | JString String
  | JNumber Double
  | JBool Bool
  | JNull
  deriving (Show)

parse :: String -> (JValue, String)
parse xs
  | Just nullValue <- parseNull xs = nullValue
  | Just boolValue <- parseBool xs = boolValue
  | Just numberValue <- parseNumber xs = numberValue
  | Just stringValue <- parseString xs = stringValue
  | Just arrayValue <- parseArray xs = arrayValue
  | Just objectValue <- parseObject xs = objectValue

parseNull :: String -> Maybe (JValue, String)
parseNull xs
  | "null" `isPrefixOf` xs = Just (JNull, drop 4 xs)
  | otherwise = Nothing

parseBool :: String -> Maybe (JValue, String)
parseBool xs
  | "true" `isPrefixOf` xs = Just (JBool True, drop 4 xs)
  | "false" `isPrefixOf` xs = Just (JBool False, drop 5 xs)
  | otherwise = Nothing

parseNumber :: String -> Maybe (JValue, String)
parseNumber xs
  | head xs == '-' || isDigit (head xs) = Just (JNumber num, rest)
  | otherwise = Nothing
  where
    (num, rest) = parseDouble xs

parseString :: String -> Maybe (JValue, String)
parseString xs
  | ('"' : xs) <- xs, (str, rest) <- parseStringInner xs = Just (JString str, rest)
  | otherwise = Nothing

parseArray :: String -> Maybe (JValue, String)
parseArray xs
  | ('[' : xs) <- xs, (']' : xs) <- xs = Just (JArray [], xs)
  | ('[' : xs) <- xs, (arr, rest) <- parseArrayInner (skipWhitespaces xs) = Just (JArray arr, rest)
  | otherwise = Nothing

parseObject :: String -> Maybe (JValue, String)
parseObject xs
  | ('{' : xs) <- xs, ('}' : xs) <- xs = Just (JObject empty, xs)
  | ('{': xs) <- xs, (map, rest) <- parseObjectInner xs = Just (JObject map, rest)
  | otherwise = Nothing

extractDigits :: String -> (String, String)
extractDigits [] = ([], [])
extractDigits (x : xs)
  | isDigit x, (rest, nextPart) <- extractDigits xs = (x : rest, nextPart)
  | otherwise = ([], x : xs)

parseDouble :: String -> (Double, String)
parseDouble xs
  | ('-' : xs) <- xs, (h, r) <- parseDouble xs = (-h, r)
  | ('.' : xs) <- afterInteger,
    (fractionalPart, rest) <- extractDigits xs =
      (read (beforeInteger ++ "." ++ fractionalPart), rest)
  | otherwise = (read beforeInteger, afterInteger)
  where
    (beforeInteger, afterInteger) = extractDigits xs

parseStringInner :: String -> (String, String)
parseStringInner xs
  | ('"' : xs) <- xs = ("", xs)
  | (x : xs) <- xs, (rest, follow) <- parseStringInner xs = (x : rest, follow)

parseArrayInner :: String -> ([JValue], String)
parseArrayInner xs
  | (',' : rest) <- rest,
    (recursed, recursedRest) <- parseArrayInner (skipWhitespaces rest) =
      (parsedItem : recursed, recursedRest)
  | (']' : rest) <- rest = ([parsedItem], rest)
  where
    (parsedItem, rest) = parse xs

parseObjectInner :: String -> (Map String JValue, String)
parseObjectInner xs
  | (',' : rest) <- rest,
    (recursed, recursedRest) <- parseObjectInner (skipWhitespaces rest) =
      (insert parsedName parsedValue recursed, recursedRest)
  | ('}' : rest) <- rest = (insert parsedName parsedValue empty, rest)
  where
    ('"' : nameStart) = xs
    (parsedName, afterName) = parseStringInner nameStart
    (':' : followingName) = skipWhitespaces afterName
    (parsedValue, afterValue) = parse followingName
    rest = skipWhitespaces afterValue

skipWhitespaces :: String -> String
skipWhitespaces [] = []
skipWhitespaces (' ' : xs) = skipWhitespaces xs
skipWhitespaces ('\n' : xs) = skipWhitespaces xs
skipWhitespaces ('\r' : xs) = skipWhitespaces xs
skipWhitespaces ('\t' : xs) = skipWhitespaces xs
skipWhitespaces xs = xs