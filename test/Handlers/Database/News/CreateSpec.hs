module Handlers.Database.News.CreateSpec where

import Control.Monad.State (State, execState, modify)
import Database.Data.FillTables (cat1, news1, news2, news3, news4, time4, user1test)
import Handlers.Database.Base (Success (..))
import Handlers.Database.News (Handle (..))
import Handlers.Database.News.Create (createNewsBase)
import qualified Handlers.Logger
import Handlers.Web.Base (NewsInternal (..))
import Schema (News (..))
import Test.Hspec
import Types (Content (..), Label (..), Login (..), Title (..))
import Schema (ColumnType (..), SortOrder (..))

spec :: Spec
spec = do
  let newsInBase = [news1, news2, news3, news4]
      logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
      baseNewsHandle =
        Handlers.Database.News.Handle
          { Handlers.Database.News.logger = logHandle,
            Handlers.Database.News.userOffset = 0,
            Handlers.Database.News.userLimit = maxBound,
            Handlers.Database.News.getTime = pure time4,
            Handlers.Database.News.findUserByLogin = \_ -> pure $ Right Nothing,
            Handlers.Database.News.sortColumnNews = DataNews,
            Handlers.Database.News.sortOrderNews = Descending,
            Handlers.Database.News.findSubString = Nothing,
            Handlers.Database.News.filtersNews = [],
            Handlers.Database.News.findCategoryByLabel = \_ -> pure $ Right Nothing,
            Handlers.Database.News.putNews =
              \(NewsInternal (MkTitle title) _login _label (MkContent content) _images ispublish) time -> do
                modify (News title time undefined undefined content ispublish :)
                pure $ Right Put,
            Handlers.Database.News.findNewsByTitle = \_ -> pure $ Right Nothing,
            Handlers.Database.News.pullAllNews = \_ _ _ _ _ _ -> pure $ Right [],
            Handlers.Database.News.editNews = \_ _ _ -> pure $ Right Change
          } :: 
            Handlers.Database.News.Handle (State [News])

  it "Success: title does not exist in the database, user exists in the database, category exists in the database" $ do
    let baseNewsHandle' =
          baseNewsHandle
            { findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right $ Just user1test),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
            }
    length (execState (createNewsBase baseNewsHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserOld") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
      `shouldBe` succ (length newsInBase)

  it "Failure: title does not exist in the database, user exists in the database, category does not exist in the database" $ do
    let baseNewsHandle' =
          baseNewsHandle
            { findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right $ Just user1test),
              findCategoryByLabel = const (pure $ Right Nothing)
            }
    length (execState (createNewsBase baseNewsHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserOld") (MkLabel "CatNew") (MkContent "Content") undefined False)) newsInBase)
      `shouldNotBe` succ (length newsInBase)

  it "Failure: title does not exist in the database, user does not exist in the database, category exists in the database" $ do
    let baseNewsHandle' =
          baseNewsHandle
            { findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
            }
    length (execState (createNewsBase baseNewsHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserNew") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
      `shouldNotBe` succ (length newsInBase)

  it "Failure: title does not exist in the database, user does not exist in the database, category does not exist in the database" $ do
    let baseNewsHandle' =
          baseNewsHandle
            { findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right Nothing)
            }
    length (execState (createNewsBase baseNewsHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserNew") (MkLabel "CatNew") (MkContent "Content") undefined False)) newsInBase)
      `shouldNotBe` succ (length newsInBase)

  it "Failure: title already exists in the database, user exists in the database, category exists in the database" $ do
    let baseNewsHandle' =
          baseNewsHandle
            { findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right $ Just user1test),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
            }
    length (execState (createNewsBase baseNewsHandle' (NewsInternal (MkTitle "OldTitle") (MkLogin "UserOld") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
      `shouldNotBe` succ (length newsInBase)

  it "Failure: title already exists in the database, user exists in the database, category does not exist in the database" $ do
    let baseNewsHandle' =
          baseNewsHandle
            { findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right $ Just user1test),
              findCategoryByLabel = const (pure $ Right Nothing)
            }
    length (execState (createNewsBase baseNewsHandle' (NewsInternal (MkTitle "OldTitle") (MkLogin "UserOld") (MkLabel "CatNew") (MkContent "Content") undefined False)) newsInBase)
      `shouldNotBe` succ (length newsInBase)

  it "Failure: title already exists in the database, user does not exist in the database, category exists in the database" $ do
    let baseNewsHandle' =
          baseNewsHandle
            { findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
            }
    length (execState (createNewsBase baseNewsHandle' (NewsInternal (MkTitle "OldTitle") (MkLogin "UserNew") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
      `shouldNotBe` succ (length newsInBase)

  it "Failure: title already exists in the database, user does not exist in the database, category does not exist in the database" $ do
    let baseNewsHandle' =
          baseNewsHandle
            { findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right Nothing)
            }
    length (execState (createNewsBase baseNewsHandle' (NewsInternal (MkTitle "OldTitle") (MkLogin "UserNew") (MkLabel "CatNew") (MkContent "Content") undefined False)) newsInBase)
      `shouldNotBe` succ (length newsInBase)

  it "Success: title does not exist in the database, user exists in the database, category exists in the database, error when working with database" $ do
    let baseNewsHandle' =
          baseNewsHandle
            { findNewsByTitle = const (pure $ Left undefined),
              findUserByLogin = const (pure $ Right $ Just user1test),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
            }
    length (execState (createNewsBase baseNewsHandle' (NewsInternal (MkTitle "NewTitle") (MkLogin "UserOld") (MkLabel "CatOld") (MkContent "Content") undefined False)) newsInBase)
      `shouldNotBe` succ (length newsInBase)
