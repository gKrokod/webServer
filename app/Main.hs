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
          Handlers.Base.getAllNews = BB.getAllNews pginfo (cLimitData cfg),
          Handlers.Base.editNews = BB.editNews pginfo
      }
  print "#########################################################################################"
  print "#########################################################################################"
  print "*******************************************************************************************************"
  print "****************************************************************************************************************"

  print "Get All News "
  a <- Handlers.Base.getAllNews baseHandle
  mapM print a

-- editNews :: ConnectionString -> Title -> UTCTime -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> IO ()
  putStrLn "\nAdd News"
  a <- Handlers.Base.createNews baseHandle "ADD NEWS 5" "login1" "Woman" "article about 5 news5" [(Image "headerNew5" "bodyNew5"),(Image "header2New5" "body2New5")] True 
  b <- Handlers.Base.createNews baseHandle "ADD NEWS 6" "login1" "Woman" "article about 6 news6" [] True 
  putStrLn "\nGet All News "
  a <- Handlers.Base.getAllNews baseHandle
  mapM print a
  putStrLn "\n Edit News6"
  -- BB.editNews pginfo "ADD NEWS 5" t (Just "EDIT NEWS 5") (Nothing) (Just "Man") Nothing [(Image "editheaderNew5" "editbodyNew5"),(Image "editheaderNew6" "editbodyNew6")] (Just False )
-- updateNews :: (Monad m) => Handle m -> Title -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> m (Either T.Text Success)
  a <- Handlers.Base.updateNews baseHandle "ADD NEWS 5" (Just "EDIT NEWS 5") (Nothing) (Just "Man") Nothing [(Image "editheaderNew5" "editbodyNew5"),(Image "editheaderNew6" "editbodyNew6")] (Just False )
  b <- Handlers.Base.updateNews baseHandle "EDIT NEWS 5" (Just "ADD NEWS 6") (Nothing) (Just "Man") Nothing [(Image "editheaderNew5" "editbodyNew5"),(Image "editheaderNew6" "editbodyNew6")] (Just False )
  c <- Handlers.Base.updateNews baseHandle "EDIT NEWS 5" (Just "EDIT NEWS 5") (Just "no autor") (Just "Man") Nothing [(Image "editheaderNew5" "editbodyNew5"),(Image "editheaderNew6" "editbodyNew6")] (Just False )
  d <- Handlers.Base.updateNews baseHandle "EDIT NEWS 5" (Just "eedit NEWS 6") (Nothing) (Just "TOtal") Nothing [(Image "editheaderNew5" "editbodyNew5"),(Image "editheaderNew6" "editbodyNew6")] (Just False )
  print a
  print b
  print c
  print d
  putStrLn "\nGet All News "
  a <- Handlers.Base.getAllNews baseHandle
  mapM print a
  pure ()

