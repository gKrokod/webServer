module Main (main) where

import Config 
-- import Database.Persist.Postgresql (ConnectionString)
import qualified Data.ByteString.Lazy as L 
import Control.Exception

--                   cfg <- L.readFile "config/db3.cfg"
-- readConf :: IO (Either IOException L.ByteString)
-- readConf = try 
--   (L.readFile "config/db3.cfg")
  

main :: IO ()
main = do 
  readConfig <- loadConfigDB
  case readConfig of
    Left e -> print e >> pure ()
    Right (c) -> do
      print "Config loaded"
      pure () 
-- configDB :: IO (Either String ConnectionString)


