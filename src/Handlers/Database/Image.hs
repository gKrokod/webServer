module Handlers.Database.Image (Handle (..)) where

import Control.Exception (SomeException)
import qualified Handlers.Logger

import Schema (Image)
import Types (NumberImage (..))


data Handle m = Handle
  { 
    logger :: Handlers.Logger.Handle m,
    pullImage :: NumberImage -> m (Either SomeException (Maybe Image))
  }        
