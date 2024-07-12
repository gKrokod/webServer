{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DeriveAnyClass #-}

module Main (main) where

import Database.Esqueleto.Experimental (Value (..))
-- import Database.Persist.Postgresql  
import GHC.Generics (Generic)
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
import Data.Aeson

data MyType = MyType T.Text 
  deriving stock (Eq, Show, Generic)
  deriving anyclass ToJSON


main :: IO ()
main = do 
  putStrLn "HEEEEREEE WE STAAAART MAINNNN"
  config <- loadConfig
-- make Tables and Fill its if need
  whenMakeTables config $ putStrLn "Make and fill Tables" 
                          >> BB.makeAndFillTables (connectionString config)
  logic config

-- instance ToJSON [User]

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
          Handlers.Base.getFullNews = BB.getFullNews pginfo (cLimitData cfg),
          Handlers.Base.editNews = BB.editNews pginfo
      }
  print "#########################################################################################"
  print "#########################################################################################"
  print "*******************************************************************************************************"
  print "****************************************************************************************************************"
  -- print "Get Full news"
  -- a <- Handlers.Base.getFullNews baseHandle "News 4 about Evil from user 1"
  -- print a
  --
  -- a <- Handlers.Base.getFullNews baseHandle "News 1 about Witch from user 1"
  -- print a
  --
  -- print "Get ALl Full news"
  -- a <- Handlers.Base.getAllNews baseHandle
  -- mapM print a
  print "getAll innerJoin news and user"
  users <- BB.getAll pginfo (cLimitData cfg)
  mapM (\(x) -> do
  -- mapM (\(Value x) -> do
          print (x) 
          putStrLn "\n") users 
  -- print "convert all users to JSON"
  -- let aa = encode @MyType (MyType "HOHO")
  -- let baa = encode @[MyType] ([MyType "HOHO", MyType "NENE", MyType "3"])
  -- print aa
  -- print baa
  -- let usersjson = mconcat $ map (encode @User) users
  -- let usersjson = (encode @User) users
  -- print usersjson 
  pure ()

-- helper xs = mconcat $ map (encode @User) xs
