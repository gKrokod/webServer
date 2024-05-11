module Main (main) where

import Config (loadConfig, ConfigDataBase, connectionString, whenMakeTables)
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
  config <- loadConfig
-- make Tables and Fill its if need
  whenMakeTables config $ putStrLn "Make Tables" >> BB.makeTables (connectionString config) >> BB.fillTables (connectionString config)

  logic config
   


logic :: ConfigDataBase -> IO () 
logic cfg = do
 putStrLn "Do Logic"
 pure ()

