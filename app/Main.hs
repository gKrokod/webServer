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
import Base.Base (Config(..), migrateDB, runAction, createUser, readUser, deleteUser)


main :: IO ()
main = do
  Prelude.putStrLn "Main start"
  config <- loadConfigDB
  case config of 
    Left decodeError -> print decodeError 
    Right cfg -> do
      print "Just cfg"
      print cfg
      let configDB = Config { configConnect = E.encodeUtf8 $ 
                              mconcat ["host=", cHost $ cfg
                                      ," port=", cPort $ cfg
                                      , " user=", cUser $ cfg
                                      , " dbname=", cDBname $ cfg
                                      , " password=", cPassword $ cfg]}
      let pGinfo = configConnect configDB
      -- create tables
      migrateDB pGinfo
      -- insert users
      u1 <- createUser pGinfo user1
      u2 <- createUser pGinfo user2
      print "YEEEEEEEEEEEEEESSSSSSSSSSS"
      -- read user
      r1 <- readUser pGinfo u1
      print r1
      print "NOOOOOOOOOOOOOOOOOOOOOOOOO"
      deleteUser pGinfo u1
      r1 <- readUser pGinfo u1
      print r1
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)
  pure ()





