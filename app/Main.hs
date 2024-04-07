module Main (main) where

import Control.Exception (bracket_)
import qualified Handlers.Logger
import qualified Handlers.WebLogic
import qualified Logger
import Network.Wai.Handler.Warp (run)
import Network.HTTP.Types (notFound404, status200, status201)
import Network.HTTP.Types.Header (hContentLength, hContentType, hLocation)
import Network.Wai (Application, responseBuilder, responseLBS, rawPathInfo)
import qualified Data.Binary.Builder as BR

greeting :: (Monad m) => Handlers.Logger.Handle m -> m ()
greeting h = do
  Handlers.Logger.logMessage h Handlers.Logger.Debug "Hello my friend"  

-- old  type Application = Request -> ResourceT IO Response
--
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived
app :: Handlers.Logger.Handle IO -> Application
app h _req f =
  bracket_
   (Handlers.Logger.logMessage h Handlers.Logger.Debug "Open app" )
   (Handlers.Logger.logMessage h Handlers.Logger.Debug "Close app" )
   (do
     let handler = Handlers.WebLogic.Handle { Handlers.WebLogic.logger = h, Handlers.WebLogic.buildResponse = responseBuilder }
     -- response <- Handlers.WebLogic.doWork handler (Handlers.WebLogic.MkRequest (rawPathInfo _req))
     -- f $ responseBuilder status200 [(hContentType, "text/plain")] (BR.fromByteString $ Handlers.WebLogic.outBody response)
     -- a <- (Handlers.WebLogic.doLogic handler (Handlers.WebLogic.MkRequest (rawPathInfo _req)))
     (Handlers.WebLogic.doLogic handler (Handlers.WebLogic.MkRequest (rawPathInfo _req))) >>= f
     -- f a
     -- response <- f <$> Handlers.WebLogic.doLogic handler (... req)
     -- f $ response
     
   
   )
     -- f $ responseLBS status200 [(hContentType, "text/plain")] "status200\n")

main :: IO ()
main = do
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = Logger.writeLog
          }
  greeting logHandle
-- run :: Port -> Application -> IO () --
--
  run 4221 (app logHandle)
  pure ()

