module Handlers.Database.Category.UpdateSpec where

import Control.Monad.State (State, execState, gets, modify)
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9)
import Handlers.Database.Base (Success (..))
import Handlers.Database.Category.Update (updateCategoryBase)
import Handlers.Database.Category (Handle (..))
import qualified Handlers.Logger
import Handlers.Web.Base (CategoryInternal (..))
import Schema (Category (..))
import Test.Hspec
import Types (Label (..))

spec :: Spec
spec = do
  let categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
                     -- "Man" "Woman" "Warrior" "Archer" "Neutral" "Evil" "Good" "Witch"
      giveParent label = case label of
        "Man" -> categoryParent cat5
        "Abstract" -> categoryParent cat2
        "Evil" -> categoryParent cat7
        _ -> error "Check baseCategoryHandle"
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
            Handlers.Database.Category.editCategory = 
               \(MkLabel label) (CategoryInternal (MkLabel newlabel) parent) -> do
                -- _categories <- get
                modify
                  ( map
                      ( \(Category l p) ->
                          if l == label
                            then Category newlabel (maybe p (giveParent . getLabel) parent)
                            else Category l (maybe p (giveParent . getLabel) parent)
                      )
                  )
                pure $ Right Change,
            Handlers.Database.Category.pullAllCategories = \_ _ -> pure $ Right []
          } ::
            Handlers.Database.Category.Handle (State [Category])

  it "Success: The category being edited exists, the new category label is not contained in the database, and the \"parent\" category is not changed." $ do
    let 
        -- archerKey = categoryParent cat5 -- Archer Man
        archerKey = categoryParent cat5
    Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
      `shouldBe` True
    Category "Archer" archerKey `elem` execState (updateCategoryBase baseCategoryHandle (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
      `shouldNotBe` True
    Category "NewArcher" archerKey `elem` execState (updateCategoryBase baseCategoryHandle (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
      `shouldBe` True

  it "Success: The category being edited exists, the new category label is not contained in the database, the \"parent\" category is being edited." $ do
    let 
        archerKey = categoryParent cat5 --  Man
        newArcherKey = categoryParent cat2 -- Abstract
    Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
      `shouldBe` True
    Category "Archer" archerKey `elem` execState (updateCategoryBase baseCategoryHandle (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") (Just . MkLabel $ "Abstract"))) categoriesInBase
      `shouldNotBe` True
    Category "NewArcher" newArcherKey `elem` execState (updateCategoryBase baseCategoryHandle (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") (Just . MkLabel $ "Abstract"))) categoriesInBase
      `shouldBe` True
    Category "NewArcher" archerKey `elem` execState (updateCategoryBase baseCategoryHandle (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") (Just . MkLabel $ "Man"))) categoriesInBase
      `shouldBe` True
  --
  it "Failure: The category being edited does not exist, the new category label is not contained in the database, and the \"parent\" category is not changed." $ do
    let 
        archerKey = categoryParent cat5 --  Man
    Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
      `shouldBe` True
    Category "Archer1" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
      `shouldNotBe` True
    Category "NewArcher" archerKey `elem` execState (updateCategoryBase baseCategoryHandle (MkLabel "Archer1") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
      `shouldNotBe` True

  it "Failure: The category being edited does not exist, the new category label is contained in the database, and the \"parent\" category is not changed." $ do
    let 
        archerKey = categoryParent cat5
        evilKey = categoryParent cat7

    Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
      `shouldBe` True
    Category "Evil" evilKey `elem` categoriesInBase --  == cat7 `elem` categoriesInBase
      `shouldBe` True
    Category "Archer" archerKey `elem` execState (updateCategoryBase baseCategoryHandle (MkLabel "Archer") (CategoryInternal (MkLabel "Evil") Nothing)) categoriesInBase
      `shouldBe` True
    Category "Evil" evilKey `elem` execState (updateCategoryBase baseCategoryHandle (MkLabel "Archer") (CategoryInternal (MkLabel "Evil") Nothing)) categoriesInBase
      `shouldBe` True

  it "Failure: The category being edited exists, the new category label is not contained in the database, and the \"parent\" category is not changed, error when working with database" $ do
    let baseCategoryHandle' = baseCategoryHandle {findCategoryByLabel = const (pure $ Left undefined)}
        archerKey = categoryParent cat5 -- Archer Man
    Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
      `shouldBe` True
    Category "Archer" archerKey `elem` execState (updateCategoryBase baseCategoryHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
      `shouldBe` True
    Category "NewArcher" archerKey `elem` execState (updateCategoryBase baseCategoryHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
      `shouldNotBe` True
