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
import qualified Database.Persist.TH as PTH
import Database.Persist.Postgresql  (ConnectionString)
import Config (loadConfigDB, ConfigDB(..))
import qualified Data.Text.Encoding as E (encodeUtf8)

import LocalTimeTemplate (localtimeTemplate)
import Base.Base (Config(..), migrateDB)

main :: IO ()
main = do
  Prelude.putStrLn "Main start"
  db <- configDB
  case db of
    Nothing -> pure ()
    Just db' -> doLogic $ configConnect db'
  -- pure ()



doLogic :: ConnectionString -> IO ()
doLogic pginfo = do
  migrateDB pginfo
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)

-- загружаем конфиг для подключения к постгрес
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
