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
         Handlers.Base.createUser = BB.createUser pginfo,
         Handlers.Base.getAllUsers = BB.getAllUsers pginfo
     }

 xs <- BB.getCategories pginfo 1 
 print "Abstract"
 let xs' = map keyValueEntityToJSON xs
 let xs'' = map entityIdToJSON xs
 print xs'
 print xs''
 xs <- BB.fetchImageBank pginfo 1 
 
 print xs
 pure ()

-- data Handle m = Handle 
--   {
--     logger :: Handlers.Logger.Handle m,
--     createUser :: Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> m (), 
--     -- createUser :: User -> m (),
--     getAllUsers :: m [User]
--     -- add some func
--   }
