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
-- import Database.Persist.Postgresql  (ConnectionString,rawExecute, insert)
-- import Database.Persist.Sql.Migration (runMigration, runSqlCommand)

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
      -- Prelude.putStrLn "Clean up" -- all row
      -- runDataBaseWithLog (configConnect db') $ cleanUp
      Prelude.putStrLn "Drop All table" -- all row
      -- runDataBaseWithLog (configConnect db') $ dropAll
      Prelude.putStrLn "Make Table in data base"
      runDataBaseWithLog (configConnect db') $ runMigration migrateAll
      doLogic $ configConnect db'
  -- pure ()


image1, image2, image3 :: Image
image1  = Image "header1" "base64 n 1" 
image2  = Image "header2" "base64 n 2" 
image3  = Image "header3" "base64 n 3" 

-- news1, news2, news3 :: News
news2 a = News "title news 2" a
news3 a = News "title news 3" a
news1 a = News "title news 1" a

cat1 :: Category
cat1 = Category "Abstract" Nothing

doLogic :: ConnectionString -> IO ()
doLogic pginfo = do
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)
  -- insert news, image, and bind
  runDataBaseWithOutLog pginfo $ do
      catId1 <- insert $ cat1
      -- catId2 <- insert $ Category "Man" (Just $ T.pack $ show catId1)
      catId2 <- insert $ Category "Man" (Just catId1)
      -- catId3 <- insert $ Category "Woman" (Just $ T.pack $ "sdf" <> show catId1)
      catId3 <- insert $ Category "Woman" (Just catId1)

      imageId1 <- insert $ image1
      -- newsId1 <- insert $ news1 (T.pack $ show catId1)
      newsId1 <- insert $ news1 catId1
      imageId2 <- insert $ image2
      -- newsId2 <- insert $ news2 (T.pack $ show catId2)
      newsId2 <- insert $ news2 catId2
      imageId3 <- insert $ image3
      newsId3 <- insert $ news3 catId1 

      _ <- insert $ ImageBank newsId1 imageId1
      _ <- insert $ ImageBank newsId1 imageId2
      _ <- insert $ ImageBank newsId2 imageId1
      _ <- insert $ ImageBank newsId3 imageId3

      -- t1 <- insert $ TestKey "testKey2" Nothing
      -- _ <- insert $ TestKey "testKey1" (Just t1)
      -- _ <- insert $ TestKey "testKey0" (Just t1) 
      pure ()
  putStrLn $ "Time to insert news, images, bind: " <> $(localtimeTemplate)
  --
  -- insert images
  -- runDataBaseWithLog pginfo $ do
  -- -- runDataBaseWithOutLog pginfo $ do
  --     -- dbruce <- insert $ Person "dBruce Wayne"
  --     -- _ <- insert $ news0
  --     _ <- insert $ news1
  --     _ <- insert $ news2
  --     pure ()
  putStrLn $ "Time to insert news: " <> $(localtimeTemplate)

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
