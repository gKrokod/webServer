module Handlers.Database.Category.CreateSpec where

import Control.Monad.State (State, execState, gets, modify)
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9)
import Handlers.Database.Base ( Success (..))
import Handlers.Database.Category (Handle (..))
import Handlers.Database.Category.Create (createCategoryBase)
import Handlers.Web.Base (CategoryInternal (..))
import qualified Handlers.Logger
import Schema (Category (..))
import Test.Hspec
import Types (Label (..))

spec :: Spec
spec = do
  let categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }
      baseCategoryHandle =
        Handlers.Database.Category.Handle
          { Handlers.Database.Category.logger = logHandle,
            Handlers.Database.Category.userOffset = 0,
            Handlers.Database.Category.userLimit = maxBound,
            Handlers.Database.Category.findCategoryByLabel =
              \(MkLabel label) -> do
                categories <- gets (map categoryLabel)
                pure $
                  Right $
                    if label `elem` categories
                      then Just (Category label undefined)
                      else Nothing,
            Handlers.Database.Category.putCategory =
               \(CategoryInternal (MkLabel label) _parent) -> do
                modify (Category label undefined :)
                pure $ Right Put,
            Handlers.Database.Category.editCategory = \_ _ -> pure $ Right Put,
            Handlers.Database.Category.pullAllCategories = \_ _ -> pure $ Right []
          } ::
            Handlers.Database.Category.Handle (State [Category])

  it "Success: category does not exist in the database, category \"parent\" exists in the database" $ do
    length (execState (createCategoryBase baseCategoryHandle (CategoryInternal (MkLabel "NewLabel") (Just $ MkLabel "Man"))) categoriesInBase)
      `shouldBe` succ (length categoriesInBase)

  it "Failure: category already exists in the database, category \"parent\" exists in the database" $ do
    length (execState (createCategoryBase baseCategoryHandle (CategoryInternal (MkLabel "Archer") (Just $ MkLabel "Man"))) categoriesInBase)
      `shouldNotBe` succ (length categoriesInBase)

  it "Failure: category does not exist in the database, category \"parent\" does not exist in the database" $ do
    length (execState (createCategoryBase baseCategoryHandle (CategoryInternal (MkLabel "NewLabel") (Just $ MkLabel "ManNew"))) categoriesInBase)
      `shouldNotBe` succ (length categoriesInBase)

  it "Failure: category already exists in the database, category \"parent\" does not exist in the database" $ do
    length (execState (createCategoryBase baseCategoryHandle (CategoryInternal (MkLabel "Man") (Just $ MkLabel "ManNew"))) categoriesInBase)
      `shouldNotBe` succ (length categoriesInBase)

  it "Failure: category does not exist in the database, category \"parent\" exists in the database, error when working with database" $ do
    let baseCategoryHandle' = baseCategoryHandle {findCategoryByLabel = const (pure $ Left undefined)}
    length (execState (createCategoryBase baseCategoryHandle' (CategoryInternal (MkLabel "NewLabel") (Just $ MkLabel "Man"))) categoriesInBase)
      `shouldNotBe` succ (length categoriesInBase)
