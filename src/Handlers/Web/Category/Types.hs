{-# LANGUAGE DuplicateRecordFields #-}

module Handlers.Web.Category.Types (CategoryInternal (..), CategoryInternalId (..)) where

import Types (Label (..), NumberCategory (..))

data CategoryInternal = CategoryInternal
  { labelCategory :: Label,
    parentCategory :: Maybe Label
  }

data CategoryInternalId = CategoryInternalId
  { idCategory :: NumberCategory,
    labelCategory :: Maybe Label,
    parentCategory :: Maybe Label
  }
