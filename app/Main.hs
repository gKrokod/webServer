module Main (main) where

import Config 
import Scheme
-- import Database.Persist.Postgresql (ConnectionString)
import qualified Data.ByteString.Lazy as L 
  

main :: IO ()
main = do 
  -- load config
  readConfig <- loadConfigDB
  case readConfig of
    Left e -> putStrLn "repair config file:" >> putStrLn e >> pure () --
    Right c -> do -- logic
      print "Config loaded"
      pure () 


