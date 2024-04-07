module Main (main) where

import qualified Handlers.Logger
import qualified Logger


greeting :: (Monad m) => Handlers.Logger.Handle m -> m ()
greeting h = do
  Handlers.Logger.logMessage h Handlers.Logger.Debug "Hello my friend"  
  
--
main :: IO ()
main = do
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = Logger.writeLog
          }
  greeting logHandle
  pure ()

