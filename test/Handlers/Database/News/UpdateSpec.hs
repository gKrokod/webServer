module Handlers.Database.News.UpdateSpec where

import Control.Monad.State (State, evalState, get)
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, news1, news2, news3, news4, time4, user1test, user2test, user3test)
import Handlers.Database.Base (Handle (..), Success (..))
import Handlers.Database.News.Update (updateNewsBase)
import qualified Handlers.Logger
import Handlers.Web.Base (NewsEditInternal (..))
import Schema (Category (..), News (..), User (..))
import Test.Hspec
import Types (Label (..), Login (..), Title (..))

spec :: Spec
spec = do
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }

      newsInBase = [news1, news2, news3, news4]
      categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
      usersInBase = [user1test, user2test, user3test]
      base = (newsInBase, usersInBase, categoriesInBase)

      baseHandle =
        Handle
          { logger = logHandle,
            findUserByLogin = \(MkLogin login) -> do
              (_n, u, _c) <- get
              let users = map userLogin u
              pure $
                Right $
                  if login `elem` users
                    then Just (User "" login undefined undefined False False undefined)
                    else Nothing,
            findNewsByTitle = \(MkTitle title) -> do
              (n, _u, _c) <- get
              let news' = map newsTitle n
              pure $
                Right $
                  if title `elem` news'
                    then Just (News title undefined undefined undefined undefined undefined)
                    else Nothing,
            findCategoryByLabel = \(MkLabel label) -> do
              (_n, _u, c) <- get
              let categories = map categoryLabel c
              pure $
                Right $
                  if label `elem` categories
                    then Just (Category label undefined)
                    else Nothing,
            getTime = pure time4,
            editNews = \_titleOld _time (NewsEditInternal _mbTitle _mbLogin _mbLabel _mbContent _images _mbPublish) -> pure $ Right Change
          } ::
          Handle (State ([News], [User], [Category]))

  it "Success: The news being edited exists, the new news title is not contained in the database" $ do
    let baseHandle' = baseHandle
    evalState
      ( updateNewsBase
          baseHandle'
          (MkTitle $ newsTitle news1)
          ( NewsEditInternal
              (Just . MkTitle $ "New Title for news1")
              Nothing
              Nothing
              Nothing
              []
              Nothing
          )
      )
      base
      `shouldBe` Right Change
  it "Success: The news being edited exists, the new news title is not contained in the database, the new user is contained in the database" $ do
    let baseHandle' = baseHandle
    evalState
      ( updateNewsBase
          baseHandle'
          (MkTitle $ newsTitle news1)
          ( NewsEditInternal
              (Just . MkTitle $ "New Title for news1")
              (Just . MkLogin $ userLogin user2test)
              Nothing
              Nothing
              []
              Nothing
          )
      )
      base
      `shouldBe` Right Change
  it "Success: The news being edited exists, the new news title is not contained in the database, the new category is contained in the database" $ do
    let baseHandle' = baseHandle
    evalState
      ( updateNewsBase
          baseHandle'
          (MkTitle $ newsTitle news1)
          ( NewsEditInternal
              (Just . MkTitle $ "New Title for news1")
              Nothing
              (Just . MkLabel $ categoryLabel cat1)
              Nothing
              []
              Nothing
          )
      )
      base
      `shouldBe` Right Change
  it "Failure: The news being edited does not exist, the new news title is not contained in the database" $ do
    let baseHandle' = baseHandle
    evalState
      ( updateNewsBase
          baseHandle'
          (MkTitle "")
          ( NewsEditInternal
              (Just . MkTitle $ "New Title for news1")
              Nothing
              Nothing
              Nothing
              []
              Nothing
          )
      )
      base
      `shouldNotBe` Right Change
  it "Failure: The news being edited exists, the new news title is contained in the database" $ do
    let baseHandle' = baseHandle
    evalState
      ( updateNewsBase
          baseHandle'
          (MkTitle $ newsTitle news1)
          ( NewsEditInternal
              (Just . MkTitle $ newsTitle news2)
              Nothing
              Nothing
              Nothing
              []
              Nothing
          )
      )
      base
      `shouldNotBe` Right Change
  it "Failure: The news being edited exists, the new news title is not contained in the database, the new user is not contained in the database" $ do
    let baseHandle' = baseHandle
    evalState
      ( updateNewsBase
          baseHandle'
          (MkTitle $ newsTitle news1)
          ( NewsEditInternal
              (Just . MkTitle $ "New Title for news1")
              (Just . MkLogin $ "")
              Nothing
              Nothing
              []
              Nothing
          )
      )
      base
      `shouldNotBe` Right Change
  it "Failure: The news being edited exists, the new news title is not contained in the database, the new category is not contained in the database" $ do
    -- updateNewsBase :: (Monad m) => Handle m -> Title -> NewsEditInternal -> m (Either T.Text Success)
    let baseHandle' = baseHandle
    evalState
      ( updateNewsBase
          baseHandle'
          (MkTitle $ newsTitle news1)
          ( NewsEditInternal
              (Just . MkTitle $ "New Title for news1")
              Nothing
              (Just . MkLabel $ "")
              Nothing
              []
              Nothing
          )
      )
      base
      `shouldNotBe` Right Change
