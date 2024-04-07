module Handlers.WebLogic where

import qualified Handlers.Logger

data Request
data Response


data Handle m = Handle
  { getRequest :: m (Request)
  , sendResponse :: Response -> m ()
  , logger :: Handlers.Logger.Handle m
  }

doWork :: (Monad m) => Handle m -> m ()
doWork h = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "doWork"  
  req <- getRequest h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get request"  
  sendResponse h undefined
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "send response"  




