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

import Data.Text
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
  insertCatTr pginfo
  -- insertCat pginfo
  -- a <- fetchCat pginfo 
  -- case a of
  --   Nothing -> undefined
  --   Just x -> do
  --     print $ categoryName x !! 0
  -- print a
  -- a <- fetchUser pginfo
  -- print a
  -- printCat  pginfo (Ca cat1)
  -- insertUsers pginfo
  -- deleteUsers pginfo
  --
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)



