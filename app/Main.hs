module Main (main) where

import Lib

-- $> print "hello!"
--
main :: IO ()
main = do
  putStrLn "132"
  putStrLn "ddd"
  let b = "Petro"
-- $> print "vovo" 
  putStrLn b
  pure ()


succ' :: Int -> Int
succ' = (+2) 

-- $> succ' 4
-- $> succ 4

