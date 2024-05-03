{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE DataKinds       #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TypeApplications #-}
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
import Data.Int (Int64)
import qualified Data.Text as T
import qualified Data.Text.Encoding as E (encodeUtf8)

import Database.Persist.Postgresql  (ConnectionString, insert, runMigration, Entity(..), SqlPersistT(..), get, toSqlKey)
import Database.Persist (SelectOpt(..), selectList)
-- import Database.Persist.Postgresql  (ConnectionString,rawExecute, insert)
-- import Database.Persist.Sql.Migration (runMigration, runSqlCommand)

import Base.Base 
import Base.BasicSchema

import Config (loadConfigDB, ConfigDB(..))
import LocalTimeTemplate (localtimeTemplate)
import Data.Time (getCurrentTime, UTCTime)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (runNoLoggingT, runStderrLoggingT, LoggingT(..), runStdoutLoggingT, NoLoggingT(..))

-- import Database.Esqueleto.Experimental (table, select, from, val, (==.), where_, (^.), (:&))
import Database.Esqueleto.Experimental

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
      runDataBaseWithLog (configConnect db') $ dropAll
      Prelude.putStrLn "Make Table in data base"
      runDataBaseWithLog (configConnect db') $ runMigration migrateAll
      doLogic $ configConnect db'
  -- pure ()

user1 :: UTCTime -> User
user1 t = User { userName = "user1"
             , userLogin = "login1"
             , userQuasiPassword = "qpass1"
             , userCreated = t
             , userIsAdmin = True
             , userIsPublisher = False }
user2 :: UTCTime -> User
user2 t = User { userName = "user2"
             , userLogin = "login2"
             , userQuasiPassword = "qpass2"
             , userCreated = t
             , userIsAdmin = True
             , userIsPublisher = True }
user3 :: UTCTime -> User
user3 t = User { userName = "user3"
             , userLogin = "login3"
             , userQuasiPassword = "qpass3"
             , userCreated = t
             , userIsAdmin = False
             , userIsPublisher = True }
cat1 :: Category
cat1 = Category "Abstract" Nothing

doLogic :: ConnectionString -> IO ()
doLogic pginfo = do
  putStrLn $ "LocalTime: " <> $(localtimeTemplate)
  time <- getCurrentTime
  
  print time
  -- insert news, image, and bind
  runDataBaseWithOutLog pginfo $ do -- fill Data Base
--- images
      imageId1 <- insert image1
      imageId2 <- insert image2
      imageId3 <- insert image3
--- users
      let u1 = user1 time
      let u2 = user2 time
      let u3 = user3 time
      userId1 <- insert u1
      userId2 <- insert u2
      userId3 <- insert u3
--- categories
      catId1 <- insert  cat1
      catId2 <- insert $ Category "Man" (Just catId1)
      catId3 <- insert $ Category "Woman" (Just catId1)
      catId4 <- insert $ Category "warrior" (Just catId2)
      catId5 <- insert $ Category "archer" (Just catId2)
      catId6 <- insert $ Category "neutral" (Just catId4)
      catId7 <- insert $ Category "evil" (Just catId4)
      catId8 <- insert $ Category "good" (Just catId4)
      catId9 <- insert $ Category "witch" (Just catId3)
--- news + image_bank
      t <- liftIO getCurrentTime
      newsId1 <- insert $ News "News about Witch from user1" t userId1 catId9 "Witch have an apple. Photo 1 and 2" [imageId1, imageId2] False
      _ <- insert $ ImageBank newsId1 imageId1
      _ <- insert $ ImageBank newsId1 imageId2
      newsId2 <- insert $ News "News about Warrior from user2" t userId2 catId4 "Warrior like Woman. No photo" [] False
      newsId3 <- insert $ News "News about Good from user3" t userId3 catId8 "Good is good. Photo 1 and 3" [imageId1, imageId3] True
      newsId4 <- insert $ News "News about Good from user1" t userId1 catId7 "Evil is evil. Photo 1" [imageId1] True
      _ <- insert $ ImageBank newsId3 imageId1
      _ <- insert $ ImageBank newsId3 imageId3
      pure ()
  print "Fill the Tables"

  print "All Users with Limit"

  runDataBaseWithOutLog pginfo $ do -- get all users with limit 
    xs <- allUsers' 
    liftIO $ mapM_ (putStrLn . show) xs 
    pure ()
  -- a <- getUser' pginfo 1
  -- print a
  --
  mb <- runDataBaseWithOutLog pginfo (getUser  1) -- get all users with limit 
  print mb
  print "Novosti davaj user 1" 
  nall <- fetchNewsUser pginfo 1
  mapM_ (putStrLn . (<> "\n") . show) nall
  
  print "Kartinky davaj  1" 
  nall <- fetchImage pginfo 1
  mapM_ (putStrLn . (<> "\n") . show) nall
  -- newsUser <- fetchRecentNewssPG pginfo
  -- mapM_ (putStrLn . (<> "\n") . show) newsUser 

  -- newsUser <- fetchNewsUser pginfo 3
  -- mapM_ (putStrLn . (<> "\n") . show) newsUser 
  print "Categorii davaj  1" 
  nall <- getCategories pginfo 2 []
  mapM_ (putStrLn . (<> "\n") . show) nall

  print "Kartinki iz novosti 1 davaj" 
  nall <- fetchImageList pginfo 1
  -- bb <- mapM (unValue) nall
  -- mapM_ (putStrLn . (<> "\n") . show) (unValue nall)
  let new = concatMap unValue nall
  print new
  -- mapM_ (putStrLn . (<> "\n") . show) (unValue nall)
  res <- mapM (runDataBaseWithOutLog pginfo . getImage . toSqlKey . unSqlBackendKey . unImageKey) new
  print res

  print "Kartinki iz novosti 3 davaj" 
  nall <- fetchImageList pginfo 1
  mapM_ (putStrLn . (<> "\n") . show) nall

  pure ()

getImage :: MonadIO m => ImageId -> SqlPersistT m (Maybe Image)
getImage n = get n
      
--
-- mnogo zaprosov k db. perepisat cherez withRecursion
-- perepisat v notacii do maybe monad -- mozno perepisat cherez unfoldr
getCategories :: ConnectionString -> Int64 -> [Category] -> IO [Category]
getCategories pginfo n acc = do
  f <- runDataBaseWithOutLog pginfo (getCategory n) -- get all users with limit 
  case f of
    Nothing -> pure acc
    Just cat -> case (categoryParent cat) of
                  Nothing -> pure (cat : acc)
                  Just n' -> getCategories pginfo (fromSqlKey n') (cat : acc)
      

getCategory :: MonadIO m => Int64 -> SqlPersistT m (Maybe Category)
getCategory n = get (toSqlKey n) 
    -- fetchAction :: (MonadIO m) => SqlPersistT m [Entity Category]
    -- fetchAction = select $ do
    --   cat1 <- from $ table @Category
    --   where_ (cat1 ^. CategoryId ==. val (toSqlKey uid))
    --   pure cat1

fetchImageBank :: ConnectionString -> Int64 -> IO [(Entity News, Entity Image)]
fetchImageBank connString uid = runDataBaseWithLog connString fetchAction
  where
    -- fetchAction :: (MonadIO m) => SqlPersistT m [Value [ImageId]]
    fetchAction = select $ do
      (news :& imagebank :& image) <- 
        from $ table @News
         `innerJoin` table @ImageBank
         `on`  (\(n :& i) -> (n ^. NewsId) ==. (i ^. ImageBankNewsId))
         `innerJoin` table @Image
         `on`  (\(_ :& i :& im) -> (i ^. ImageBankImageId) ==. (im ^. ImageId))
      pure $ (news, image)

fetchNewsUser :: ConnectionString -> Int64 -> IO [Entity News]
fetchNewsUser connString uid = runDataBaseWithLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Entity News]
    fetchAction = select $ do
      news <- from $ table @News
      where_ (news ^. NewsUserId ==. val (toSqlKey uid))
      pure news

-- fetchImageFromNews :: ConnectionString -> Int64 -> IO [Entity Image]
-- fetchImageFromNews connString uid = runDataBaseWithLog connString fetchAction
--   where
--     fetchAction :: (MonadIO m) => SqlPersistT m [Entity Image]
--     fetchAction = 
--       select $ do
--       (news :& images) <- from $ table @News
--         `leftJoin` table @Image
--         `on` (\(news :& images) -> images ^. ImageId `in_`  (news ^. NewsImagesIds))
--         -- `on` (\(news :& images) -> images ^. ImageId `in_` news ^. NewsImagesIds)
--       pure images

fetchImageList :: ConnectionString -> Int64 -> IO [Value [ImageId]]
fetchImageList connString uid = runDataBaseWithLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Value [ImageId]]
    fetchAction = select $ do
      news <- from $ table @News
      where_ (news ^. NewsId ==. val (toSqlKey uid))
      pure $ news ^. NewsImagesIds

-- fetchImageFromNews :: ConnectionString -> Int64 -> IO [Entity Image]
-- fetchImageFromNews connString uid = runDataBaseWithLog connString fetchAction
--   where
--     fetchAction :: (MonadIO m) => SqlPersistT m [Entity Image]
--     fetchAction = select $ do
--       news <- from $ table @News
--       where_ (news ^. NewsId ==. val (toSqlKey uid))
--       let list = (news ^. NewsImagesIds)
--       image <- from $ table @Image
--       -- where_ (image ^. ImageId ==. val (toSqlKey uid))
--       -- where_ (image ^. ImageId `in_` valList (map toSqlKey [1,2,3]) )-- news ^. NewsImagesIds)
--       where_ (image ^. ImageId `in_` (list))
--       pure image

fetchImage :: ConnectionString -> Int64 -> IO [Entity Image]
fetchImage connString uid = runDataBaseWithLog connString fetchAction
  where
    fetchAction :: (MonadIO m) => SqlPersistT m [Entity Image]
    fetchAction = select $ do
      image <- from $ table @Image
      where_ (image ^. ImageId ==. val (toSqlKey uid))
      pure image

getUser :: MonadIO m => Int64 -> SqlPersistT m (Maybe User)
getUser n = get (toSqlKey n) 
--
-- allUsers :: MonadIO m => SqlPersistT m [Entity User]
-- allUsers = select $ from pure
--
allUsers' :: MonadIO m => SqlPersistT m [Entity User]
allUsers' = selectList [] [LimitTo 1]
--
-- allImages :: MonadIO m => SqlPersistT m [Entity Image]
-- allImages = select $ from pure
--
-- allCategories :: MonadIO m => SqlPersistT m [Entity Category]
-- allCategories = select $ from pure

allNews :: MonadIO m => SqlPersistT m [Entity News]
allNews = select $ do
  news <- from $ table @News
  pure news

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
--
image1, image2, image3 :: Image
image1  = Image "header1" "base64 n 1" 
image2  = Image "header2" "base64 n 2" 
image3  = Image "header3" "base64 n 3" 
