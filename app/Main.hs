{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingStrategies #-}
--
--
-- {-# LANGUAGE MultiParamTypeClasses      #-}
-- {-# LANGUAGE GADTs                      #-}
-- {-# LANGUAGE GeneralizedNewtypeDeriving #-}
-- {-# LANGUAGE RecordWildCards            #-}
-- {-# LANGUAGE FlexibleInstances          #-}
-- {-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TemplateHaskellQuotes #-}

module Main (main) where
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (encodeUtf8)

import Database.Persist.Postgresql  (ConnectionString, insert, runMigration)

import Base.Base 
import Base.BasicSchema

import Config (loadConfigDB, ConfigDB(..))
import LocalTimeTemplate (localtimeTemplate)

main :: IO ()
main = do
  Prelude.putStrLn "Main start"
  db <- configDB
  case db of
    Nothing -> pure ()
    Just db' -> do 
      runDataBaseWithLog (configConnect db') $ runMigration migrateAll
      doLogic $ configConnect db'
  -- pure ()



doLogic :: ConnectionString -> IO ()
doLogic pginfo = do
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)

  runDataBaseWithOutLog pginfo $ do
      -- dbruce <- insert $ Person "dBruce Wayne"
      pure ()
     
  runDataBaseWithOutLog pginfo $ do
      -- bruce <- insert $ Person "Bruce Wayne"
      -- michael <- insert $ Person "Michael"
      -- target <- insert $ Store "Target"
      -- gucci <- insert $ Store "Gucci"
      -- sevenEleven <- insert $ Store "7-11"
      --
      -- insert $ PersonStore bruce gucci
      -- insert $ PersonStore bruce sevenEleven
      --
      -- insert $ PersonStore michael target
      -- insert $ PersonStore michael sevenEleven
      pure ()
  -- pure ()
  --
--
configDB :: IO (Maybe Config)
configDB = do
  config <- loadConfigDB
  case config of 
    Left decodeError -> print decodeError >> pure Nothing
    Right cfg -> do
      print "Config DataBase is loaded"
      let configDB = Config { configConnect = E.encodeUtf8 $ 
                              mconcat ["host=", cHost $ cfg
                                      ," port=", cPort $ cfg
                                      , " user=", cUser $ cfg
                                      , " dbname=", cDBname $ cfg
                                      , " password=", cPassword $ cfg]}
      pure $ Just configDB
