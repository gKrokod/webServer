module Main (main) where

import Config 
import Scheme
-- import Database.Persist.Postgresql (ConnectionString)
import qualified Data.ByteString.Lazy as L 
import qualified Handlers.Logger
import qualified Handlers.Base
import qualified Base.Base as BB 
-- import Database.Persist.Postgresql  (ConnectionString, runMigration)

main :: IO ()
main = do 
  putStrLn "Main Start"
  -- load config
  config <- loadConfigDB
  case config of
    Left e -> putStrLn "repair config file:" >> putStrLn e >> pure () --
    Right cfg -> do
      putStrLn "Make Tables"
      BB.makeTables (connectionString cfg)
      logic cfg

logic :: ConfigDataBase -> IO () 
logic cfg = do
 putStrLn "Do Logic"
 pure ()

