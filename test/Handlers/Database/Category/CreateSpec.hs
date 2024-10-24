module Handlers.Database.Category.CreateSpec where

import Control.Monad.State (State, execState, gets, modify)
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, time4)
import Handlers.Database.Base (Handle (..), Success (..))
import Handlers.Database.Category.Create (createCategoryBase)
import qualified Handlers.Logger
import Handlers.Web.Base (CategoryInternal (..))
import Schema (Category (..))
import Test.Hspec
import Types (Label (..))

spec :: Spec
spec = do
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }
      categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
      -- "Man" "Woman" "Warrior" "Archer" "Neutral" "Evil" "Good" "Witch"
      baseHandle =
        Handle
          { logger = logHandle,
            findCategoryByLabel = \(MkLabel label) -> do
              categories <- gets (map categoryLabel)
              pure $
                Right $
                  if label `elem` categories
                    then Just (Category label undefined)
                    else Nothing,
            getTime = pure time4,
            putCategory = \(CategoryInternal (MkLabel label) parent) -> do
              modify (Category label undefined :)
              pure $ Right Put
          } ::
          Handle (State [Category])

  it "Success: category does not exist in the database, category \"parent\" exists in the database" $ do
    let baseHandle' = baseHandle
    length (execState (createCategoryBase baseHandle' (CategoryInternal (MkLabel "NewLabel") (Just $ MkLabel "Man"))) categoriesInBase)
      `shouldBe` succ (length categoriesInBase)

  it "Failure: category already exists in the database, category \"parent\" exists in the database" $ do
    let baseHandle' = baseHandle
    length (execState (createCategoryBase baseHandle' (CategoryInternal (MkLabel "Archer") (Just $ MkLabel "Man"))) categoriesInBase)
      `shouldNotBe` succ (length categoriesInBase)

  it "Failure: category does not exist in the database, category \"parent\" does not exist in the database" $ do
    let baseHandle' = baseHandle
    length (execState (createCategoryBase baseHandle' (CategoryInternal (MkLabel "NewLabel") (Just $ MkLabel "ManNew"))) categoriesInBase)
      `shouldNotBe` succ (length categoriesInBase)

  it "Failure: category already exists in the database, category \"parent\" does not exist in the database" $ do
    let baseHandle' = baseHandle
    length (execState (createCategoryBase baseHandle' (CategoryInternal (MkLabel "Man") (Just $ MkLabel "ManNew"))) categoriesInBase)
      `shouldNotBe` succ (length categoriesInBase)

  it "Failure: category does not exist in the database, category \"parent\" exists in the database, error when working with database" $ do
    let baseHandle' = baseHandle {findCategoryByLabel = const (pure $ Left undefined)}
    length (execState (createCategoryBase baseHandle' (CategoryInternal (MkLabel "NewLabel") (Just $ MkLabel "Man"))) categoriesInBase)
      `shouldNotBe` succ (length categoriesInBase)
