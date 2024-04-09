module Handlers.WebLogic where

import qualified Data.ByteString as B
import qualified Data.Text as T
import qualified Handlers.Logger
import Network.HTTP.Types.Header (hContentType)
import Network.HTTP.Types (notFound404, status200, status201, Status, ResponseHeaders)
import Data.Binary.Builder as BR (fromByteString, Builder)
import Network.Wai (Request, Response, rawPathInfo)


data Handle m = Handle
  { logger :: Handlers.Logger.Handle m,
    buildResponse :: Status -> ResponseHeaders -> Builder -> Response
  }

-- old  type Application = Request -> ResourceT IO Response
--
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived
--

doLogic :: (Monad m) => Handle m -> Request -> m (Response) 
doLogic h req = do
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "get request"  
  Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug (T.pack $ show req)
  -- Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "send response"  
  pure $ buildResponse h status200 [(hContentType, "text/plain")] (BR.fromByteString $ (rawPathInfo req))
