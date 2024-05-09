module Handlers.Logger (Handle (..), Log (..), logMessage) where

import qualified Data.Text as T

data Log = Debug | Warning | Error | Fatal deriving (Eq, Ord, Show)

data Handle m = Handle
  { levelLogger :: Log,
    writeLog :: T.Text -> m ()
  }

logMessage :: (Monad m) => Handle m -> Log -> T.Text -> m ()
logMessage h lvl msg
  | lvl >= (levelLogger h) = writeLog h (mconcat ["[", T.pack $ show lvl, "] ", msg])
  | otherwise = pure ()

