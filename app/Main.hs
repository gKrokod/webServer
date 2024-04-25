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
-- import Data.Tree
-- import Data.Time.Calendar
import Base.Category
import Data.Time


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
-- make tables
  migrateDB pginfo
-- -- insert Dictionary in DB
--   _ <- insertDictionary pginfo catTr1 
--   dictionary <- fetchDictionary pginfo
-- -- fetch dictionary from DB and insert Categories in DB
--   case dictionary of
--     Nothing -> print "ne nashli dictionary"
--     Just cdict -> do 
--       let spisok = treeToList (categoryDictionaryTree cdict); 
--       insertCategories pginfo spisok
-- -- insert News and Users in DB
--   insertNews pginfo
--   insertUsers pginfo
--
  -- filterUser pginfo
  -- runAction pginfo $ do
  --   -- people <- selectList [UserData_created ==. fromGregorian 1987 01 01 ] []
  --   people <- selectList [UserData_created >=. fromGregorian 1200 01 01 ] [Desc UserLogin, LimitTo 6]
  --   liftIO $ print people
    -- maybePerson <- getBy $ UniqueLogin "user4"
    -- case maybePerson of
    --     Nothing -> liftIO $ putStrLn "Ничего нет"
    --     Just person -> liftIO $ print person 
  pure ()
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)


filterUser pginfo = 
  runAction pginfo $ do
    -- people <- selectList [UserData_created ==. fromGregorian 1987 01 01 ] []
    people <- selectList [UserData_created >=. fromGregorian 1200 01 01 ] [Desc UserLogin, LimitTo 6]
    liftIO $ print people


fetchUserhelp pginfo =
  runAction pginfo $ do
    maybePerson <- getBy $ UniqueLogin "user4"
    case maybePerson of
        Nothing -> liftIO $ putStrLn "Ничего нет"
        Just person -> liftIO $ print person 
