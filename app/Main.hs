module Main (main) where

import Config (loadConfig, ConfigDataBase, connectionString, whenMakeTables, cLimitData)
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
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

main :: IO ()
main = do 
  putStrLn "HEEEEREEE WE STAAAART MAINNNN"
  config <- loadConfig
-- make Tables and Fill its if need
  whenMakeTables config $ putStrLn "Make and fill Tables" 
                          >> BB.makeAndFillTables (connectionString config)
  logic config

logic :: ConfigDataBase -> IO () 
logic cfg = do
  let pginfo = connectionString cfg
  putStrLn "Do Logic"
  t <- getCurrentTime
  Logger.writeLog ( T.pack $ show t )
  let logHandle = Handlers.Logger.Handle
        { Handlers.Logger.levelLogger = Debug,
          Handlers.Logger.writeLog = Logger.writeLog
        }
  let baseHandle = Handlers.Base.Handle
        { Handlers.Base.logger = logHandle,
------------------------------ user end point
          Handlers.Base.putUser = BB.putUser pginfo,
          Handlers.Base.findUserByLogin = BB.findUserByLogin pginfo,
          Handlers.Base.getTime = getCurrentTime,
          Handlers.Base.getAllUsers = BB.getAllUsers pginfo (cLimitData cfg),
--------------------------------
------------------------------ category end point
          Handlers.Base.findCategoryByLabel = BB.findCategoryByLabel pginfo,
          Handlers.Base.putCategory = BB.putCategory pginfo,
          Handlers.Base.changeCategory = BB.changeCategory pginfo,
          Handlers.Base.getBranchCategories = BB.getBranchCategories pginfo (cLimitData cfg),
          Handlers.Base.getAllCategories = BB.getAllCategories pginfo (cLimitData cfg),
--------------------------------
------------------------------ image end points
          Handlers.Base.getImage = BB.getImage pginfo,
          Handlers.Base.putImage = BB.putImage pginfo
      }
  print "#########################################################################################"
  print "#########################################################################################"
  print "*******************************************************************************************************"
  print "****************************************************************************************************************"

  print "Get News 1 "
  a <- BB.getNews' pginfo 1
  print a

  print "Get Full News 1 "
  a <- BB.getFullNews pginfo 1
  print a
  -- a <- BB.fetchImageBank pginfo 1
  -- mapM_ print a
  -- print "getOne and Two"
  -- a <- Handlers.Base.getImage baseHandle 1
  -- b <- Handlers.Base.getImage baseHandle 2
  -- mapM_ print [a,b]
  -- -- pure
  -- print "insert 4 and 5"
  -- Handlers.Base.putImage baseHandle "4444" "Base64 4444"
  -- Handlers.Base.putImage baseHandle "header 5" "base64 5555"
  -- a <- Handlers.Base.getImage baseHandle 4
  -- b <- Handlers.Base.getImage baseHandle 5
  -- mapM_ print [a,b]

  pure ()
