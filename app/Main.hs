module Main (main) where

import Config (loadConfig, ConfigDataBase, connectionString, whenMakeTables)
import Scheme
-- import Database.Persist.Postgresql (ConnectionString)
import qualified Data.ByteString.Lazy as L 
import qualified Handlers.Logger
import Handlers.Logger (Log(Debug))
import qualified Handlers.Base
import qualified Base.Base as BB 
import qualified Logger
import Database.Persist.Postgresql  (keyValueEntityToJSON, ConnectionString, runMigration, entityIdToJSON)
import Data.Time (getCurrentTime)

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
  let pginfo = connectionString cfg
  putStrLn "Do Logic"
  let logHandle = Handlers.Logger.Handle
        { Handlers.Logger.levelLogger = Debug,
          Handlers.Logger.writeLog = Logger.writeLog
        }
  let baseHandle = Handlers.Base.Handle
        { Handlers.Base.logger = logHandle,
          Handlers.Base.putUser = BB.putUser pginfo,
          Handlers.Base.findUserByLogin = BB.findUserByLogin pginfo,
          Handlers.Base.getTime = getCurrentTime,
          Handlers.Base.getAllUsers = BB.getAllUsers pginfo
      }
  cuser <- Handlers.Base.createUser baseHandle "Vova1" "LOGIN1" "pssss"  True True
  case cuser of
    Left e -> print e
    Right _ -> print "User Create"
--  xs <- BB.getCategories pginfo 1 
--  print "Abstract"
--  let xs' = map keyValueEntityToJSON xs
--  let xs'' = map entityIdToJSON xs
--  print xs'
--  print xs''
--  xs <- BB.fetchImageBank pginfo 1 
--  
--  print xs
  pure ()

