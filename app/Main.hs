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
import Language.Haskell.TH
import Control.Monad.Reader
import Control.Monad.Logger 
import qualified Database.Persist.TH as PTH
import Database.Persist.Postgresql 
import Data.Aeson
import GHC.Generics (Generic)
import qualified Data.ByteString.Lazy.Char8 as BC
import qualified Data.ByteString.Lazy as L
import Config (loadConfigDB, ConfigDB(..))
-- import qualified Data.ByteString.Lazy.Char8 as LC
import qualified Data.Text.Encoding as E (encodeUtf8)
import LocalTimeTemplate

import Base.BasicSchema
import Base.Base-- (Config(..), migrateDB, runAction, createUser, readUser, deleteUser)
import Base.TestEntity
import Data.Tree
import Data.Time.Calendar
import Base.Category


main :: IO ()
main = do
  Prelude.putStrLn "Main start"
  config <- loadConfigDB
  case config of 
    Left decodeError -> print decodeError 
    Right cfg -> do
      print "Config DataBase is loaded"
      -- print cfg
      let configDB = Config { configConnect = E.encodeUtf8 $ 
                              mconcat ["host=", cHost $ cfg
                                      ," port=", cPort $ cfg
                                      , " user=", cUser $ cfg
                                      , " dbname=", cDBname $ cfg
                                      , " password=", cPassword $ cfg]}
      -- do logic
      logic (configConnect configDB)      
  pure ()


logic :: ConnectionString -> IO ()
logic pginfo = do
  migrateDB pginfo
  -- insertAll pginfo
  deleteAll pginfo
  n2 <- fetchNews pginfo
  a <- fetchTree pginfo 
  case a of
    Nothing -> print a--liftIO $ undefined 
    Just x -> do
      print "Just"
      putStr $ drawTree $ fmap (show . T.unpack) $ categoryDictionaryTree x
      -- print x 
  case n2 of
    Nothing -> print n2
    Just x -> do
      print "just News"
      -- print $ addDays 1230 (newsData_created x)
      -- print x
  pure ()
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)



