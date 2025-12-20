module Main where

import MyLib (parse)

main :: IO ()
main = do
  print (parse "null")
  print (parse "true")
  print (parse "false")
  print (parse "-123")
  print (parse "\"hello world\"")
  print (parse "[1,true,3,4]")
  print (parse "{\"items\":[1, 2, 3, [true, false]]}")
