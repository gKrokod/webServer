{-# LANGUAGE TemplateHaskell #-}
module Handlers.Database.News.CreateSpec  where

import Handlers.Database.News.Create (createNewsBase)
import Test.Hspec
import Handlers.Database.Base (Handle (..), Success(..))
import Database.Data.LocalTime (localtimeTemplate)
import qualified Handlers.Logger
import Database.Data.FillTables (news1, news2, news3, news4, cat1, user1)
import Handlers.Web.Base (NewsInternal (..))
import Scheme (News(..))
import Control.Monad.State (State, execState, modify, gets)
import Types (Title(..), Content(..), Label(..), Login(..))

spec :: Spec
spec = do
    let logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
        newsInBase = [news1, news2, news3, news4]
        baseHandle =
          Handle
            { logger = logHandle,
              findCategoryByLabel = undefined,
              findUserByLogin = undefined,
              findNewsByTitle = undefined,
              getTime = pure (read $(localtimeTemplate)),
              putNews = \(NewsInternal (MkTitle title) login label (MkContent content) _images ispublish) time -> do
                modify (News title time undefined undefined content ispublish :)
                pure $ Right Put
            } ::
            Handle (State [News])

    it "Success: title does not exist in the database, user exists in the database, category exists in the database" $ do
      let baseHandle' =
            baseHandle
              { findNewsByTitle = const (pure $ Right Nothing),
                findUserByLogin = const (pure $ Right $ Just user1),
                findCategoryByLabel = const (pure $ Right $ Just cat1)
              }
      length (execState (createNewsBase baseHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserOld") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
        `shouldBe` succ (length newsInBase)

    it "Failure: title does not exist in the database, user exists in the database, category does not exist in the database" $ do
      let baseHandle' =
            baseHandle
              { findNewsByTitle = const (pure $ Right Nothing),
                findUserByLogin = const (pure $ Right $ Just user1),
                findCategoryByLabel = const (pure $ Right Nothing)
              }
      length (execState (createNewsBase baseHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserOld") (MkLabel "CatNew") (MkContent "Content") undefined False)) newsInBase)
        `shouldNotBe` succ (length newsInBase)

    it "Failure: title does not exist in the database, user does not exist in the database, category exists in the database" $ do
      let baseHandle' =
            baseHandle
              { findNewsByTitle = const (pure $ Right Nothing),
                findUserByLogin = const (pure $ Right Nothing),
                findCategoryByLabel = const (pure $ Right $ Just cat1)
              }
      length (execState (createNewsBase baseHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserNew") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
        `shouldNotBe` succ (length newsInBase)

    it "Failure: title does not exist in the database, user does not exist in the database, category does not exist in the database" $ do
      let baseHandle' =
            baseHandle
              { findNewsByTitle = const (pure $ Right Nothing),
                findUserByLogin = const (pure $ Right Nothing),
                findCategoryByLabel = const (pure $ Right Nothing)
              }
      length (execState (createNewsBase baseHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserNew") (MkLabel "CatNew") (MkContent "Content") undefined False)) newsInBase)
        `shouldNotBe` succ (length newsInBase)

    it "Failure: title already exists in the database, user exists in the database, category exists in the database" $ do
      let baseHandle' =
            baseHandle
              { findNewsByTitle = const (pure $ Right $ Just news1),
                findUserByLogin = const (pure $ Right $ Just user1),
                findCategoryByLabel = const (pure $ Right $ Just cat1)
              }
      length (execState (createNewsBase baseHandle' (NewsInternal (MkTitle "OldTitle") (MkLogin "UserOld") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
        `shouldNotBe` succ (length newsInBase)

    it "Failure: title already exists in the database, user exists in the database, category does not exist in the database" $ do
      let baseHandle' =
            baseHandle
              { findNewsByTitle = const (pure $ Right $ Just news1),
                findUserByLogin = const (pure $ Right $ Just user1),
                findCategoryByLabel = const (pure $ Right Nothing)
              }
      length (execState (createNewsBase baseHandle' (NewsInternal (MkTitle "OldTitle") (MkLogin "UserOld") (MkLabel "CatNew") (MkContent "Content") undefined False)) newsInBase)
        `shouldNotBe` succ (length newsInBase)

    it "Failure: title already exists in the database, user does not exist in the database, category exists in the database" $ do
      let baseHandle' =
            baseHandle
              { findNewsByTitle = const (pure $ Right $ Just news1),
                findUserByLogin = const (pure $ Right Nothing),
                findCategoryByLabel = const (pure $ Right $ Just cat1)
              }
      length (execState (createNewsBase baseHandle' (NewsInternal (MkTitle "OldTitle") (MkLogin "UserNew") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
        `shouldNotBe` succ (length newsInBase)

    it "Failure: title already exists in the database, user does not exist in the database, category does not exist in the database" $ do
      let baseHandle' =
            baseHandle
              { findNewsByTitle = const (pure $ Right $ Just news1),
                findUserByLogin = const (pure $ Right Nothing),
                findCategoryByLabel = const (pure $ Right Nothing)
              }
      length (execState (createNewsBase baseHandle' (NewsInternal (MkTitle "OldTitle") (MkLogin "UserNew") (MkLabel "CatNew") (MkContent "Content") undefined False)) newsInBase)
        `shouldNotBe` succ (length newsInBase)

    it "Success: title does not exist in the database, user exists in the database, category exists in the database, error when working with database" $ do
      let baseHandle' =
            baseHandle
              { findNewsByTitle = const (pure $ Left undefined),
                findUserByLogin = const (pure $ Right $ Just user1),
                findCategoryByLabel = const (pure $ Right $ Just cat1)
              }
      length (execState (createNewsBase baseHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserOld") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
        `shouldNotBe` succ (length newsInBase)
