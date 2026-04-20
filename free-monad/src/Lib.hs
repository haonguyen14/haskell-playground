{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib
  ( readFileM,
    writeFileM,
    executeIO,
    executePrint,
    Ops,
  )
where

import Control.Monad.Free
import qualified Data.ByteString as BS

data Ops next
  = ReadFile FilePath (BS.ByteString -> next)
  | WriteFile FilePath BS.ByteString next
  deriving (Functor)

readFileM :: FilePath -> Free Ops BS.ByteString
readFileM path = liftF (ReadFile path id)

writeFileM :: FilePath -> BS.ByteString -> Free Ops ()
writeFileM path bs = liftF (WriteFile path bs ())

executeIO :: Free Ops a -> IO a
executeIO (Pure a) = return a
executeIO (Free (ReadFile path next)) = do
  content <- BS.readFile path
  executeIO (next content)
executeIO (Free (WriteFile path bs next)) = do
  BS.writeFile path bs
  executeIO next

executePrint :: Free Ops a -> IO a
executePrint (Pure a) = return a
executePrint (Free (ReadFile path next)) = do
  putStrLn $ "reading file " ++ show path
  executePrint (next "Mock file content")
executePrint (Free (WriteFile path bs next)) = do
  putStrLn $ "writing file " ++ show path ++ " with content " ++ show bs
  executePrint next
