module Main (main) where

import Config (loadConfig, ConfigDataBase, connectionString, whenMakeTables)
import Scheme
-- import Database.Persist.Postgresql (ConnectionString)
import qualified Data.ByteString.Lazy as L 
import qualified Handlers.Logger
import qualified Handlers.Base
import qualified Base.Base as BB 
import Database.Persist.Postgresql  (keyValueEntityToJSON, ConnectionString, runMigration, entityIdToJSON)

main :: IO ()
main = do 
  putStrLn "Main Start"
  config <- loadConfig
-- make Tables and Fill its if need
  whenMakeTables config $ putStrLn "Make and fill Tables" 
                          >> BB.makeAndFillTables (connectionString config)
  logic config

logic :: ConfigDataBase -> IO () 
logic cfg = do
 putStrLn "Do Logic"
 let pginfo = connectionString cfg
 xs <- BB.getCategories pginfo 1 
 print "Abstract"
 let xs' = map keyValueEntityToJSON xs
 let xs'' = map entityIdToJSON xs
 print xs'
 print xs''
 xs <- BB.fetchImageBank pginfo 1 
 
 print xs
 pure ()

