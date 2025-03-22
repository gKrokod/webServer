module Handlers.Web.Image (Handle (..)) where

import qualified Handlers.Database.Image
import qualified Handlers.Logger

data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    base :: Handlers.Database.Image.Handle m
  }
