module Main (main) where

import Config (configDB)
-- import Database.Persist.Postgresql (ConnectionString)

main :: IO ()
main = do 
  readConfig <- configDB
  case readConfig of
    Left e -> print "Error load config" >> pure ()
    Right connectString -> do
      print "Config loaded"
      pure () 
-- configDB :: IO (Either String ConnectionString)


