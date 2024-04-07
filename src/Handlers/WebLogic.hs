module Handlers.WebLogic where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Handlers.Logger
import Network.HTTP.Types.Header
import Network.HTTP.Types
import Data.Binary.Builder as BR
import Network.Wai

data MyRequest = MkRequest { myRawPathInfo :: B.ByteString }
data MyResponse = MkResponse { outBody :: B.ByteString }

-- data Request
-- data Response


data Handle m = Handle
  -- { request :: MyRequest 
  -- , sendResponse :: Response -> m ()
  { logger :: Handlers.Logger.Handle m,
    buildResponse :: Status -> ResponseHeaders -> Builder -> Response
  }

-- old  type Application = Request -> ResourceT IO Response
--
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived
--
doWork :: (Monad m) => Handle m -> MyRequest -> m (MyResponse) 
-- doWork :: (Monad m) => Handle m -> MyRequest -> m (MyResponse) 
doWork h req = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "doWork"  
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get request"  
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show $ myRawPathInfo req)
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "send response"  
  pure (MkResponse "my response\n")


   -- (f $ responseLBS status200 [(hContentType, "text/plain")] "status200\n")

     -- f $ responseBuilder status200 [(hContentType, "text/plain")] (BR.fromByteString $ Handlers.WebLogic.outBody response)

doLogic :: (Monad m) => Handle m -> MyRequest -> m (Response) 
-- doWork :: (Monad m) => Handle m -> MyRequest -> m (MyResponse) 
doLogic h req = do
  mr <- doWork h req
  pure $ buildResponse h status200 [(hContentType, "text/plain")] (BR.fromByteString $ outBody mr)

