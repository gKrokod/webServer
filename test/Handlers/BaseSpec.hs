{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Handlers.BaseSpec (spec) where

import Handlers.Base 

import Test.Hspec (describe, it, Spec, shouldBe, shouldNotBe)
import Scheme (User(..), Category (..), News(..))
import Base.FillTables (user1, user2, user3, cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat9,cat8, news1,news2,news3,news4)
import Base.LocalTime (localtimeTemplate)
import Data.List(foldr1)
import qualified Handlers.Logger 
import qualified Logger 
import Control.Monad.State (evalState, State, get, execState, modify)
import Control.Monad.Identity (Identity, runIdentity)
import qualified Data.Text as T
import Test.QuickCheck (property)
import Data.Time (UTCTime)
import Data.Maybe (mapMaybe, listToMaybe)

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
      it "Sucess: user does not exist in the database" $ do
          let baseHandle' = baseHandle {findUserByLogin = const (pure $ Right Nothing)}
          length (execState (createUserBase baseHandle' "Name" "Login" "Password" False False) usersInBase)
           `shouldBe` 
              (succ $ length usersInBase)
      it "Failure: user exists in the database" $ do
          let baseHandle' = baseHandle {findUserByLogin = const (pure $ Right $ Just user1)}
          length (execState (createUserBase baseHandle' "Name" "Login" "Password" False False) usersInBase)
           `shouldNotBe` 
              (succ $ length usersInBase)

      it "Failure: error when working with database" $ do
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

      it "Success: category does not exist in the database, category \"parent\" exists in the database" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "NewLabel" (Just "Man")) categoriesInBase)
           `shouldBe` 
              (succ $ length categoriesInBase)

      it "Failure: category already exists in the database, category \"parent\" exists in the database" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "Archer" (Just "Man")) categoriesInBase)
           `shouldNotBe` 
              (succ $ length categoriesInBase)

      it "Failure: category does not exist in the database, category \"parent\" does not exist in the database" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "NewLabel" (Just "ManNew")) categoriesInBase)
           `shouldNotBe` 
              (succ $ length categoriesInBase)

      it "Failure: category already exists in the database, category \"parent\" does not exist in the database" $ do
          let baseHandle' = baseHandle 
          length (execState (createCategoryBase baseHandle' "Man" (Just "ManNew")) categoriesInBase)
           `shouldNotBe` 
              (succ $ length categoriesInBase)

      it "Failure: category does not exist in the database, category \"parent\" exists in the database, error when working with database" $ do
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


      it "Success: title does not exist in the database, user exists in the database, category exists in the database" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right $ Just user1),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
                                        }
          length (execState (createNewsBase baseHandle' "NewTitle" "UserOld" "CatOld" "Content" undefined False) newsInBase)
           `shouldBe` 
              (succ $ length newsInBase)

      it "Failure: title does not exist in the database, user exists in the database, category does not exist in the database" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right $ Just user1),
              findCategoryByLabel = const (pure $ Right Nothing)
                                        }
          length (execState (createNewsBase baseHandle' "NewTitle" "UserOld" "CatNew" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "Failure: title does not exist in the database, user does not exist in the database, category exists in the database" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
                                        }
          length (execState (createNewsBase baseHandle' "NewTitle" "UserNew" "CatOld" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "Failure: title does not exist in the database, user does not exist in the database, category does not exist in the database" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right Nothing),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right Nothing)
                                        }
          length (execState (createNewsBase baseHandle' "NewTitle" "UserNew" "CatNew" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "Failure: title already exists in the database, user exists in the database, category exists in the database" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right $ Just user1),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
                                        }
          length (execState (createNewsBase baseHandle' "OldTitle" "UserOld" "CatOld" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "Failure: title already exists in the database, user exists in the database, category does not exist in the database" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right $ Just user1),
              findCategoryByLabel = const (pure $ Right Nothing)
                                        }
          length (execState (createNewsBase baseHandle' "OldTitle" "UserOld" "CatNew" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "Failure: title already exists in the database, user does not exist in the database, category exists in the database" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right $ Just cat1)
                                        }
          length (execState (createNewsBase baseHandle' "OldTitle" "UserNew" "CatOld" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "Failure: title already exists in the database, user does not exist in the database, category does not exist in the database" $ do
          let baseHandle' = baseHandle {
              findNewsByTitle = const (pure $ Right $ Just news1),
              findUserByLogin = const (pure $ Right Nothing),
              findCategoryByLabel = const (pure $ Right Nothing)
                                        }
          length (execState (createNewsBase baseHandle' "OldTitle" "UserNew" "CatNew" "Content" undefined False) newsInBase)
           `shouldNotBe` 
              (succ $ length newsInBase)

      it "Success: title does not exist in the database, user exists in the database, category exists in the database, error when working with database" $ do
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
      let giveParent label = case label of
                               "Man" -> categoryParent cat5
                               "Abstract" -> categoryParent cat2
                               "Evil" -> categoryParent cat7
                               _ -> undefined
      let baseHandle  = Handle
            {
               logger = logHandle,
               findCategoryByLabel = \label  -> do
                 categories <- map categoryLabel <$> get
                 pure $ Right $ 
                   if label `elem` categories then Just (Category label undefined)
                                              else Nothing,
               editCategory = \label newlabel parent -> do
                                 categories <- get
                                 modify (map (\(Category l p) -> if l == label 
                                                                    then Category newlabel (maybe p giveParent parent)
                                                                    else Category l (maybe p giveParent parent) ))
                                 pure $ Right Change
                                                      }  :: Handle (State [Category])
      it "Success: The category being edited exists, the new category label is not contained in the database, and the \"parent\" category is not changed." $ do 
          let baseHandle' = baseHandle 
          let archerKey = giveParent "Man"  -- Archer Man
          (Category "Archer" archerKey) `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
           `shouldBe`  True
          (Category "Archer" archerKey) `elem` (execState (updateCategoryBase baseHandle' "Archer" "NewArcher" Nothing) categoriesInBase)
           `shouldNotBe`  True
          (Category "NewArcher" archerKey) `elem` (execState (updateCategoryBase baseHandle' "Archer" "NewArcher" Nothing) categoriesInBase)
           `shouldBe`  True

      it "Success: The category being edited exists, the new category label is not contained in the database, the \"parent\" category is being edited." $ do 
          let baseHandle' = baseHandle 
          let archerKey = giveParent "Man"   --  Man
          let newArcherKey = giveParent "Abstract"  -- Abstract
          (Category "Archer" archerKey) `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
           `shouldBe`  True
          (Category "Archer" archerKey) `elem` (execState (updateCategoryBase baseHandle' "Archer" "NewArcher" (Just "Abstract")) categoriesInBase)
           `shouldNotBe`  True
          (Category "NewArcher" newArcherKey) `elem` (execState (updateCategoryBase baseHandle' "Archer" "NewArcher" (Just "Abstract")) categoriesInBase)
           `shouldBe`  True
          (Category "NewArcher" archerKey) `elem` (execState (updateCategoryBase baseHandle' "Archer" "NewArcher" (Just "Man")) categoriesInBase)
           `shouldBe`  True

      it "Failure: The category being edited does not exist, the new category label is not contained in the database, and the \"parent\" category is not changed." $ do 
          let baseHandle' = baseHandle 
          let archerKey = giveParent "Man"   --  Man

          (Category "Archer" archerKey) `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
           `shouldBe`  True
          (Category "Archer1" archerKey) `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
           `shouldNotBe`  True
          (Category "NewArcher" archerKey) `elem` (execState (updateCategoryBase baseHandle' "Archer1" "NewArcher" Nothing) categoriesInBase)
           `shouldNotBe`  True

      it "Failure: The category being edited does not exist, the new category label is contained in the database, and the \"parent\" category is not changed." $ do 
          let baseHandle' = baseHandle
          let archerKey = giveParent "Man"
          let evilKey = giveParent "Evil"

          (Category "Archer" archerKey) `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
           `shouldBe`  True
          (Category "Evil" evilKey) `elem` categoriesInBase --  == cat7 `elem` categoriesInBase
           `shouldBe`  True
          (Category "Archer" archerKey) `elem` (execState (updateCategoryBase baseHandle' "Archer" "Evil" Nothing) categoriesInBase)
           `shouldBe`  True
          (Category "Evil" evilKey) `elem` (execState (updateCategoryBase baseHandle' "Archer" "Evil" Nothing) categoriesInBase)
           `shouldBe`  True

      it "Failure: The category being edited exists, the new category label is not contained in the database, and the \"parent\" category is not changed, error when working with database" $ do 
          let baseHandle' = baseHandle {findCategoryByLabel = const (pure $ Left undefined)}
          let archerKey = giveParent "Man"  -- Archer Man
          (Category "Archer" archerKey) `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
           `shouldBe`  True
          (Category "Archer" archerKey) `elem` (execState (updateCategoryBase baseHandle' "Archer" "NewArcher" Nothing) categoriesInBase)
           `shouldBe`  True
          (Category "NewArcher" archerKey) `elem` (execState (updateCategoryBase baseHandle' "Archer" "NewArcher" Nothing) categoriesInBase)
           `shouldNotBe`  True
--
--
  describe "Edit News" $ do
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
             } 
              

      let newsInBase = [news1,news2,news3,news4]
      let categoriesInBase = [cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9]
      let usersInBase = [user1,user2,user3]
      let base = (newsInBase, usersInBase, categoriesInBase)

      let baseHandle  = Handle
              { 
           
               logger = logHandle,
               findUserByLogin = \login  -> do
                 (n,u,c) <- get
                 let users = map userLogin u
                 pure $ Right $ 
                   if login `elem` users then Just (User "" login undefined undefined False False)
                                              else Nothing,
               findNewsByTitle = \title  -> do
                 (n,u,c) <- get
                 let news' = map newsTitle n
                 pure $ Right $ 
                   if title `elem` news' then Just (News title undefined undefined undefined undefined undefined)
                                              else Nothing,
               findCategoryByLabel = \label  -> do
                 (n,u,c) <- get
                 let categories = map categoryLabel c
                 pure $ Right $ 
                   if label `elem` categories then Just (Category label undefined)
                                              else Nothing,
               getTime = pure (read $(localtimeTemplate)), 
               editNews = \titleOld time mbTitle mbLogin mbLabel mbContent images mbPublish -> pure $ Right Change 
                                                      } :: Handle (State ([News],[User],[Category]))

      it "Success: The news being edited exists, the new news title is not contained in the database" $ do 
          let baseHandle' = baseHandle
          (evalState (updateNews baseHandle' 
                                         (newsTitle news1) (Just "New Title for news1") 
                                         Nothing Nothing Nothing 
                                          [] Nothing) base)
           `shouldBe`  (Right Change)
      it "Success: The news being edited exists, the new news title is not contained in the database, the new user is contained in the database" $ do 
          let baseHandle' = baseHandle
          (evalState (updateNews baseHandle' 
                                         (newsTitle news1) (Just "New Title for news1") 
                                         (Just $ userLogin user2) Nothing Nothing 
                                          [] Nothing) base)
           `shouldBe`  (Right Change)
      it "Success: The news being edited exists, the new news title is not contained in the database, the new category is contained in the database" $ do 
          let baseHandle' = baseHandle
          (evalState (updateNews baseHandle' 
                                         (newsTitle news1) (Just "New Title for news1") 
                                         Nothing (Just $ categoryLabel cat1)  Nothing 
                                          [] Nothing) base)
           `shouldBe`  (Right Change)
      it "Failure: The news being edited does not exist, the new news title is not contained in the database" $ do 
          let baseHandle' = baseHandle
          (evalState (updateNews baseHandle' 
                                         "" (Just "New Title for news1") 
                                         Nothing Nothing Nothing 
                                          [] Nothing) base)
           `shouldNotBe`  (Right Change)
      it "Failure: The news being edited exists, the new news title is contained in the database" $ do 
          let baseHandle' = baseHandle
          (evalState (updateNews baseHandle' 
                                         (newsTitle news1) (Just $ newsTitle news2)
                                         Nothing Nothing Nothing 
                                          [] Nothing) base)
           `shouldNotBe`  (Right Change)
      it "Failure: The news being edited exists, the new news title is not contained in the database, the new user is not contained in the database" $ do 
          let baseHandle' = baseHandle
          (evalState (updateNews baseHandle' 
                                         (newsTitle news1) (Just "New Title for news1") 
                                         (Just "") Nothing Nothing 
                                          [] Nothing) base)
            `shouldNotBe`  (Right Change)
      it "Failure: The news being edited exists, the new news title is not contained in the database, the new category is not contained in the database" $ do 
          let baseHandle' = baseHandle
          (evalState (updateNews baseHandle' 
                                         (newsTitle news1) (Just "New Title for news1") 
                                         Nothing (Just "") Nothing 
                                          [] Nothing) base)
           `shouldNotBe`  (Right Change)

  describe "Get privilege (admin, publisher) for User from Base" $ do

      let usersInBase = [user1,user2,user3]
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
      let usersInBase = [user1, user2, user3] 
      let baseHandle  = Handle
            {
               logger = logHandle,
               findUserByLogin = \login -> do
                 users <- get
                 pure 
                   (Right 
                     $ listToMaybe 
                      $ mapMaybe (\user@(User _ l _ _ _ _) -> if l == login
                                                            then Just user else Nothing) $ users)
                                                      }  :: Handle (State [User])
      it "Get no privilege for a user that is not in the database" $ do
             let baseHandle' = baseHandle
             (evalState (getPrivilege baseHandle' "NoUser") usersInBase)
                `shouldBe` 
                   Right (False, False)

      it "Get privilege for a user that is in the database" $ do
             let baseHandle' = baseHandle
             (evalState (getPrivilege baseHandle' (userLogin user1)) usersInBase)
                `shouldBe` 
                   Right (userIsAdmin user1, userIsPublisher user1)
             let baseHandle' = baseHandle
             (evalState (getPrivilege baseHandle' (userLogin user2)) usersInBase)
                `shouldBe` 
                   Right (userIsAdmin user2, userIsPublisher user2)
             let baseHandle' = baseHandle
             (evalState (getPrivilege baseHandle' (userLogin user3)) usersInBase)
                `shouldBe` 
                   Right (userIsAdmin user3, userIsPublisher user3)
