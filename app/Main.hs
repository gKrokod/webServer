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
          Handlers.Base.putImage = BB.putImage pginfo,
--------------------------------
------------------------------ news 
          Handlers.Base.putNews = BB.putNews pginfo,
          Handlers.Base.findNewsByTitle = BB.findNewsByTitle pginfo,
          Handlers.Base.getAllNews = BB.getAllNews pginfo (cLimitData cfg)

      }
  print "#########################################################################################"
  print "#########################################################################################"
  print "*******************************************************************************************************"
  print "****************************************************************************************************************"

  print "Get All News "
  a <- Handlers.Base.getAllNews baseHandle
  mapM print a

  print "\nAdd News"
  a <- Handlers.Base.createNews baseHandle "ADD NEWS 5" "login1" "Woman" "article about 5 news5" [(Image "headerNew5" "bodyNew5"),(Image "headerNew6" "bodyNew6")] True 
  b <- Handlers.Base.createNews baseHandle "ADD NEWS 6" "login1" "Woman" "article about 6 news6" [(Image "headerNew5" "bodyNew5"),(Image "headerNew6" "bodyNew6")] True 


  print "\nGet All News "
  a <- Handlers.Base.getAllNews baseHandle
  mapM print a
  pure ()
