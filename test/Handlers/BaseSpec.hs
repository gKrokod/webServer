{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Handlers.BaseSpec (spec) where

import Test.Hspec
import Scheme
import Handlers.Base 
import Base.FillTables (user1, user2, user3, cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat9,cat8, news1,news2,news3,news4)
import Base.LocalTime (localtimeTemplate)
import Data.List(foldr1)
import qualified Handlers.Logger 
import qualified Logger 
import Control.Monad.State
import Control.Monad.Identity
import qualified Data.Text as T
import Test.QuickCheck
import Data.Time (UTCTime)

spec :: Spec
spec = do
  describe "Data should be limited (Panigate)" $ do
      let serverLimit = 15
      let numberUserInBase = 27
      let baseHandle  = Handle
            {pullAllUsers = \userOffset userLimit -> pure $ Right   
                                                    $ take (min userLimit serverLimit) 
                                                    $ drop userOffset 
                                                    $ map (const (User "" "" undefined undefined False False))
                                                      [1..numberUserInBase]}  :: Handle Identity
      it "Random offset and limit" $ do
        property $ \offset limit -> do
          let offset' = max 0 offset
          let limit' =  max 0 limit
          let baseHandle' = baseHandle {userOffset = offset', userLimit = limit'}
          length <$> (runIdentity $ getAllUsers baseHandle')
           `shouldBe` 
              Right (foldr1 min [max 0 (numberUserInBase - userOffset baseHandle'), serverLimit, userLimit baseHandle'] )

  describe "Create User" $ do
      -- let serverLimit = 15
      -- let numberUserInBase = 27
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
      let usersInBase = [user1, user2, user3] 
      let baseHandle  = Handle
            {
               logger = logHandle,
               findUserByLogin = undefined,
               getTime = pure (read $(localtimeTemplate)), 
               putUser = \name login pass time admin publish -> do
                            modify ((User name login undefined time admin publish):)
                            pure $ Right Put
                                                      }  :: Handle (State [User])
      it "Sucess add user : user don't exist" $ do
          let baseHandle' = baseHandle {findUserByLogin = const (pure $ Right Nothing)}
          length (execState (createUserBase baseHandle' "Name" "Login" "Password" False False) usersInBase)
           `shouldBe` 
              (succ $ length usersInBase)
      it "not add user : user already exist" $ do
          let baseHandle' = baseHandle {findUserByLogin = const (pure $ Right $ Just user1)}
          length (execState (createUserBase baseHandle' "Name" "Login" "Password" False False) usersInBase)
           `shouldNotBe` 
              (succ $ length usersInBase)

      it "not add user : fail data base" $ do
          let baseHandle' = baseHandle {findUserByLogin = const (pure $ Left undefined)}
          length (execState (createUserBase baseHandle' "Name" "Login" "Password" False False) usersInBase)
           `shouldNotBe` 
              (succ $ length usersInBase)

  describe "Create Category" $ do
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
      let categoriesInBase = [cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9]
-- "Man" "Woman" "Warrior" "Archer" "Neutral" "Evil" "Good" "Witch"
      let baseHandle  = Handle
            {
               logger = logHandle,
               findCategoryByLabel = \label  -> do
                 categories <- map categoryLabel <$> get
                 pure $ Right $ 
                   if label `elem` categories then Just (Category label undefined)
                                              else Nothing,
               getTime = pure (read $(localtimeTemplate)), 
               putCategory = \label parent -> do
                            modify ((Category label undefined):)
                            pure $ Right Put
                                                      }  :: Handle (State [Category])

      it "Success add category : label new, parent exist" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "NewLabel" (Just "Man")) categoriesInBase)
           `shouldBe` 
              (succ $ length categoriesInBase)

      it "not add category : label old, parent exist" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "Archer" (Just "Man")) categoriesInBase)
           `shouldNotBe` 
              (succ $ length categoriesInBase)

      it "not add category : label new, parent don't exist" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "NewLabel" (Just "ManNew")) categoriesInBase)
           `shouldNotBe` 
              (succ $ length categoriesInBase)

      it "not add category : label old, parent don't exist" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "Man" (Just "ManNew")) categoriesInBase)
           `shouldNotBe` 
              (succ $ length categoriesInBase)

      it "not add category : label new, parent exist, fail data base" $ do
          let baseHandle' = baseHandle {findCategoryByLabel = const (pure $ Left undefined)}
          length (execState (createCategoryBase baseHandle' "NewLabel" (Just "Man")) categoriesInBase)
           `shouldNotBe` 
              (succ $ length categoriesInBase)

  describe "Create News" $ do
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
      let newsInBase = [news1,news2,news3,news4]
      let baseHandle  = Handle
            {
               logger = logHandle,
               findCategoryByLabel = undefined, 
               findUserByLogin = undefined,
               findNewsByTitle = undefined,
               getTime = pure (read $(localtimeTemplate)), 
               putNews = \title time login label content _images ispublish -> do
                            modify ((News title time undefined undefined content ispublish):)
                            pure $ Right Put
                                                      }  :: Handle (State [News])

      it "Success add news : title new, user exist, category exist" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right $ Just user1),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
                                        }
          length (execState (createNewsBase baseHandle' "NewTitle" "UserOld" "CatOld" "Content" undefined False) newsInBase)
           `shouldBe` 
              (succ $ length newsInBase)

      it "not add news : title new, user exist, category don't exist" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right $ Just user1),
              findCategoryByLabel = const (pure $ Right Nothing)
                                        }
          length (execState (createNewsBase baseHandle' "NewTitle" "UserOld" "CatNew" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "not add news : title new, user don't exist, category exist" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
                                        }
          length (execState (createNewsBase baseHandle' "NewTitle" "UserNew" "CatOld" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "not add news : title new, user don't exist, category don't exist" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right Nothing)
                                        }
          length (execState (createNewsBase baseHandle' "NewTitle" "UserNew" "CatNew" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "not add news : title old, user exist, category exist" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right $ Just user1),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
                                        }
          length (execState (createNewsBase baseHandle' "OldTitle" "UserOld" "CatOld" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "not add news : title old, user exist, category don't exist" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right $ Just user1),
              findCategoryByLabel = const (pure $ Right Nothing)
                                        }
          length (execState (createNewsBase baseHandle' "OldTitle" "UserOld" "CatNew" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "not add news : title old, user don't exist, category exist" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
                                        }
          length (execState (createNewsBase baseHandle' "OldTitle" "UserNew" "CatOld" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "not add news : title old, user don't exist, category don't exist" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right Nothing)
                                        }
          length (execState (createNewsBase baseHandle' "OldTitle" "UserNew" "CatNew" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "not add news : title new, user exist, category exist, fail data base" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Left undefined),
              findUserByLogin = const (pure $ Right $ Just user1),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
                                        }
          length (execState (createNewsBase baseHandle' "NewTitle" "UserOld" "CatOld" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

  describe "Edit Category" $ do
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
      let categoriesInBase = [cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9]
-- "Man" "Woman" "Warrior" "Archer" "Neutral" "Evil" "Good" "Witch"
      let baseHandle  = Handle
            {
               logger = logHandle,
               findCategoryByLabel = \label  -> do
                 categories <- map categoryLabel <$> get
                 pure $ Right $ 
                   if label `elem` categories then Just (Category label undefined)
                                              else Nothing,
               getTime = pure (read $(localtimeTemplate)), 
               putCategory = \label parent -> do
                            modify ((Category label undefined):)
                            pure $ Right Put
                                                      }  :: Handle (State [Category])

      it "Success edit category : label... old, parent exist" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "NewLabel" (Just "Man")) categoriesInBase)
           `shouldBe` 
              (succ $ length categoriesInBase)

      it "not add category : label old, parent exist" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "Archer" (Just "Man")) categoriesInBase)
           `shouldNotBe` 
              (succ $ length categoriesInBase)
--
--
--
  describe "Part 2Handlers.Base" $ do
    -- context "Logic base" $ do
      it "removes leading and trailing whitespace" $ do
        (succ 2 :: Int) `shouldBe` (3 :: Int)
      it "removes leading and trailing whitespace" $ do
        (succ 2 :: Int) `shouldBe` (2 :: Int)

--   let logHandle = Handlers.Logger.Handle
--         { Handlers.Logger.levelLogger = Debug,
--           Handlers.Logger.writeLog = Logger.writeLog
--         }
--   let baseHandle = Handlers.Base.Handle
--         { Handlers.Base.logger = logHandle,
--           Handlers.Base.putUser = BB.putUser pginfo,
--           Handlers.Base.findUserByLogin = BB.findUserByLogin pginfo,
--           Handlers.Base.getTime = getCurrentTime,
--           Handlers.Base.makeHashPassword = Base.Crypto.makeHashPassword,
--           Handlers.Base.validPassword = BB.validPassword pginfo,
--           Handlers.Base.validCopyRight = BB.validCopyRight pginfo,
-- -- default setup
--           Handlers.Base.userOffset = 0,
--           Handlers.Base.userLimit = maxBound,
--           Handlers.Base.sortColumnNews = DataNews,
--           Handlers.Base.sortOrderNews = Descending,
--           Handlers.Base.findSubString = Nothing,
--           Handlers.Base.filtersNews = [],
-- -- default *
--           Handlers.Base.pullAllUsers = BB.pullAllUsers pginfo (cLimitData cfg),
--           Handlers.Base.findCategoryByLabel = BB.findCategoryByLabel pginfo,
--           Handlers.Base.putCategory = BB.putCategory pginfo,
--           Handlers.Base.editCategory = BB.editCategory pginfo,
--           Handlers.Base.pullAllCategories = BB.pullAllCategories pginfo (cLimitData cfg),
--           Handlers.Base.pullImage = BB.pullImage pginfo,
--           Handlers.Base.putNews = BB.putNews pginfo,
--           Handlers.Base.findNewsByTitle = BB.findNewsByTitle pginfo,
--           Handlers.Base.pullAllNews = BB.pullAllNews pginfo (cLimitData cfg),
--           Handlers.Base.editNews = BB.editNews pginfo
--       }
-- data Handle m = Handle 
--   {
-- --API
--     logger :: Handlers.Logger.Handle m,
--     userOffset :: Int, --default
--     userLimit :: Int, -- default
--     sortColumnNews :: ColumnType, -- default
--     sortOrderNews :: SortOrder, -- default
--     findSubString :: Maybe Find,
--     filtersNews :: [FilterItem],
--     --
--     getTime :: m UTCTime,
--     makeHashPassword :: PasswordUser -> UTCTime -> HashPasswordUser,
--     validPassword :: Login -> PasswordUser -> m (Either SomeException Bool),
--     validCopyRight :: Login -> Title-> m (Either SomeException Bool),
-- -- pullAllUsers :: Offset -> Limit -> m (Either SomeException [User])
--     pullAllUsers :: Offset -> Limit -> m (Either SomeException [User]),
-- -- getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
--     pullAllNews :: Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> m (Either SomeException [NewsOut]),
-- -- getAllNews :: (Monad m) => Handle m -> m (Either T.Text [NewsOut])
--     pullAllCategories :: Offset -> Limit -> m (Either SomeException [Category]),
-- -- getAllCategories :: (Monad m) => Handle m -> m (Either T.Text [Category])
--     pullImage :: NumberImage -> m (Either SomeException (Maybe Image)),
--
--     findUserByLogin :: Login -> m (Either SomeException (Maybe User)), 
--     findCategoryByLabel :: Label -> m (Either SomeException (Maybe Category)),
--     findNewsByTitle :: Title -> m (Either SomeException (Maybe News)), 
--
--     putUser :: Name -> Login -> PasswordUser -> UTCTime -> Bool -> Bool -> m (Either SomeException Success), 
-- -- createUserBase :: (Monad m) => Handle m -> Name -> Login -> PasswordUser -> Bool -> Bool -> m (Either T.Text Success)  
--     putCategory :: Label -> Maybe Label -> m (Either SomeException Success), 
-- -- createCategoryBase :: (Monad m) => Handle m -> Label -> Maybe Label -> m (Either T.Text Success) 
--     -- putNews :: Title -> UTCTime -> Login -> Label -> Content -> [Image] -> Bool -> m (),
--     putNews :: Title -> UTCTime -> Login -> Label -> Content -> [Image] -> Bool -> m (Either SomeException Success),
-- -- createNewsBase :: (Monad m) => Handle m -> Title -> Login -> Label -> Content -> [Image] -> Bool -> m (Either T.Text Success) 
--
--     editNews :: Title -> UTCTime -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> m (Either SomeException Success), 
-- -- updateNews :: (Monad m) => Handle m -> Title -> Maybe Title -> Maybe Login -> Maybe Label -> Maybe Content -> [Image] -> Maybe Bool -> m (Either T.Text Success)
--     editCategory :: Label -> NewLabel -> Maybe Label -> m (Either SomeException Success)
-- -- updateCategory :: (Monad m) => Handle m -> Label -> NewLabel -> Maybe Label -> m (Either T.Text Success)  
--   }
--       let baseHandle = Handle
--            { logger = undefined,
--             putUser = undefined, -- BB.putUser pginfo,
--             findUserByLogin = undefined, -- BB.findUserByLogin pginfo,
--             getTime = undefined, -- getCurrentTime,
--             makeHashPassword= undefined, -- Base.Crypto.makeHashPassword,
--             validPassword = undefined, -- BB.validPassword pginfo,
--             validCopyRight = undefined, -- BB.validCopyRight pginfo,
--             userOffset = 0,
--             userLimit = maxBound,
--             sortColumnNews = undefined, -- DataNews,
--             sortOrderNews = undefined, -- Descending,
--             findSubString = Nothing, --undefined, -- Nothing,
--             filtersNews = undefined, -- [],
--             pullAllUsers = \userOffset userLimit -> take (min limit serverLimit) $ drop userOffset [1..], -- BB.pullAllUsers pginfo (cLimitData cfg),
-- --     pullAllUsers :: Offset -> Limit -> m (Either SomeException [User]),
-- -- getAllUsers :: (Monad m) => Handle m -> m (Either T.Text [User])
-- -- getAllUsers h = do
-- --   let logHandle = logger h
-- --   logMessage logHandle Debug "Try to get all users from database"
-- --   users <- pullAllUsers h (userOffset h) (userLimit h)
-- --   when (isLeft users) (logMessage logHandle Handlers.Logger.Error "function pullAllUsers fail")
-- --   pure $ either (Left . T.pack . displayException) Right users 
--             findCategoryByLabel = undefined, -- BB.findCategoryByLabel pginfo,
--             putCategory = undefined, -- BB.putCategory pginfo,
--             editCategory = undefined, -- BB.editCategory pginfo,
--             pullAllCategories = undefined, -- BB.pullAllCategories pginfo (cLimitData cfg),
--             pullImage = undefined, -- BB.pullImage pginfo,
--             putNews = undefined, -- BB.putNews pginfo,
--             findNewsByTitle = undefined, -- BB.findNewsByTitle pginfo,
--             pullAllNews = undefined, -- BB.pullAllNews pginfo (cLimitData cfg),
--             editNews = undefined -- BB.editNews pginfo
--         } :: Handle (State (Either T.Text [Int]))
