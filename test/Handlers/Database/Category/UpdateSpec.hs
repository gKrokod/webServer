{-# LANGUAGE TemplateHaskell #-}

module Handlers.Database.Category.UpdateSpec  where

import Handlers.Database.Category.Update (updateCategoryBase)
import Test.Hspec
import Handlers.Database.Base (Handle (..), Success(..))
import Database.Data.LocalTime (localtimeTemplate)
import qualified Handlers.Logger
import Database.Data.FillTables (cat1, cat2, cat3, cat4, cat5,  cat6, cat7, cat8, cat9)
import Handlers.Web.Base (CategoryInternal (..))
import Schema (Category(..))
import Control.Monad.State (State, execState, modify, gets, get)
import Types (Label(..))

spec :: Spec
spec = do
    let logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
        categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]
        -- "Man" "Woman" "Warrior" "Archer" "Neutral" "Evil" "Good" "Witch"
        giveParent label = case label of
          "Man" -> categoryParent cat5
          "Abstract" -> categoryParent cat2
          "Evil" -> categoryParent cat7
          _ -> undefined
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
              editCategory = \(MkLabel label) (CategoryInternal (MkLabel newlabel) parent) -> do
                categories <- get
                modify
                  ( map
                      ( \(Category l p) ->
                          if l == label
                            then Category newlabel (maybe p (giveParent . getLabel) parent)
                            else Category l (maybe p (giveParent . getLabel) parent)
                      )
                  )
                pure $ Right Change
            } ::
            Handle (State [Category])
    it "Success: The category being edited exists, the new category label is not contained in the database, and the \"parent\" category is not changed." $ do
      let baseHandle' = baseHandle
          archerKey = giveParent "Man" -- Archer Man
      Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
        `shouldBe` True
      Category "Archer" archerKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
        `shouldNotBe` True
      Category "NewArcher" archerKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
        `shouldBe` True

    it "Success: The category being edited exists, the new category label is not contained in the database, the \"parent\" category is being edited." $ do
      let baseHandle' = baseHandle
          archerKey = giveParent "Man" --  Man
          newArcherKey = giveParent "Abstract" -- Abstract
      Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
        `shouldBe` True
      Category "Archer" archerKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") (Just . MkLabel $ "Abstract"))) categoriesInBase
        `shouldNotBe` True
      Category "NewArcher" newArcherKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") (Just . MkLabel $ "Abstract"))) categoriesInBase
        `shouldBe` True
      Category "NewArcher" archerKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") (Just . MkLabel $ "Man"))) categoriesInBase
        `shouldBe` True

    it "Failure: The category being edited does not exist, the new category label is not contained in the database, and the \"parent\" category is not changed." $ do
      let baseHandle' = baseHandle
          archerKey = giveParent "Man" --  Man
      Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
        `shouldBe` True
      Category "Archer1" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
        `shouldNotBe` True
      Category "NewArcher" archerKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer1") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
        `shouldNotBe` True

    it "Failure: The category being edited does not exist, the new category label is contained in the database, and the \"parent\" category is not changed." $ do
      let baseHandle' = baseHandle
          archerKey = giveParent "Man"
          evilKey = giveParent "Evil"

      Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
        `shouldBe` True
      Category "Evil" evilKey `elem` categoriesInBase --  == cat7 `elem` categoriesInBase
        `shouldBe` True
      Category "Archer" archerKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "Evil") Nothing)) categoriesInBase
        `shouldBe` True
      Category "Evil" evilKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "Evil") Nothing)) categoriesInBase
        `shouldBe` True

    it "Failure: The category being edited exists, the new category label is not contained in the database, and the \"parent\" category is not changed, error when working with database" $ do
      let baseHandle' = baseHandle {findCategoryByLabel = const (pure $ Left undefined)}
          archerKey = giveParent "Man" -- Archer Man
      Category "Archer" archerKey `elem` categoriesInBase --  == cat5 `elem` categoriesInBase
        `shouldBe` True
      Category "Archer" archerKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
        `shouldBe` True
      Category "NewArcher" archerKey `elem` execState (updateCategoryBase baseHandle' (MkLabel "Archer") (CategoryInternal (MkLabel "NewArcher") Nothing)) categoriesInBase
        `shouldNotBe` True
