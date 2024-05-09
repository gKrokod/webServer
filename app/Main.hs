module Main (main) where

import Config 
import Scheme
-- import Database.Persist.Postgresql (ConnectionString)
import qualified Data.ByteString.Lazy as L 
import qualified Handlers.Logger
import qualified Handlers.Base
  

main :: IO ()
main = do 
  -- load config
  readConfig <- loadConfigDB
  case readConfig of
    Left e -> putStrLn "repair config file:" >> putStrLn e >> pure () --
    Right c -> do -- logic
      print "Config loaded"
      pure () 


