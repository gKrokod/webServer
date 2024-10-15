module Handlers.Web.Category.Types (CategoryInternal (..)) where

import Types (Label (..))

data CategoryInternal = CategoryInternal
  { labelCategory :: Label,
    parentCategory :: Maybe Label
  }
