{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
module Handlers.Logger (Handle (..), Log (..), logMessage) where

import qualified Data.Text as T
import GHC.Generics (Generic)
import Data.Aeson (FromJSON (..), ToJSON (..))

data Log = Debug | Warning | Error | Fatal 
  deriving stock (Eq, Ord, Show, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Handle m = Handle
  { levelLogger :: Log,
    writeLog :: T.Text -> m ()
  }

logMessage :: (Monad m) => Handle m -> Log -> T.Text -> m ()
logMessage h lvl msg
  | lvl >= levelLogger h = writeLog h (mconcat ["[", T.pack $ show lvl, "] ", msg])
  | otherwise = pure ()
