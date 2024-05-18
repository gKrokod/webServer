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
          Handlers.Base.getAllCategories = BB.getAllCategories pginfo (cLimitData cfg)
--
--------------------------------
          -- Handlers.Base.panigate = cLimitData cfg, -- somnitelno
      }
  print "START users:"
  alluser <- Handlers.Base.getAllUsers baseHandle 
  mapM_ print alluser
  cuser <- Handlers.Base.createUser baseHandle "Vova1" "LOGIN1" "pssss"  True True
  case cuser of
    Left e -> print e
    Right a -> print a >> print "User Create"
  print "FINISH users:"
  alluser <- Handlers.Base.getAllUsers baseHandle 
  mapM_ print alluser
--  xs <- BB.getCategories pginfo 1 
--  print "Abstract"
--  let xs' = map keyValueEntityToJSON xs
--  let xs'' = map entityIdToJSON xs
--  print xs'
--  print xs''
--  xs <- BB.fetchImageBank pginfo 1 
--  
--  print xs
  print "START Categories:"
  allcategories <- Handlers.Base.getAllCategories baseHandle 
  mapM_ print allcategories
  print "ADD TWO Categories:"
  Handlers.Base.createCategory baseHandle "Dark" (Just "Warrior") --(Just "Woman" :: Handlers.Base.Label)
  Handlers.Base.createCategory baseHandle "ONO" Nothing --(Just "Woman" :: Handlers.Base.Label)
  allcategories <- Handlers.Base.getAllCategories baseHandle 
  mapM_ print allcategories
  print "Update TWO Categories:"
  -- Handlers.Base.updateCategory baseHandle "Dark" "Light" (Just "Warrior") --(Just "Woman" :: Handlers.Base.Label)
  Handlers.Base.updateCategory baseHandle "Dark" "Light" Nothing --(Just "Woman" :: Handlers.Base.Label)
  Handlers.Base.updateCategory baseHandle "ONO" "ONO2" (Just "Man") --(Just "Woman" :: Handlers.Base.Label)
  allcategories <- Handlers.Base.getAllCategories baseHandle 
  mapM_ print allcategories
  pure ()

