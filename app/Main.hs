module Main (main) where

import Control.Exception (bracket_)
import qualified Handlers.Logger
import qualified Handlers.WebLogic
import qualified Logger
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, responseBuilder)

-- old  type Application = Request -> ResourceT IO Response
--
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived

app :: Handlers.WebLogic.Handle IO -> Application
app h req f =
  bracket_
   (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "Open app" )
   (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "Close app")
   (Handlers.WebLogic.doLogic h req >>= f)

main :: IO ()
main = do
  putStrLn "Welcome"

  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = Logger.writeLog
          }
  let handle = Handlers.WebLogic.Handle { 
                 Handlers.WebLogic.logger = logHandle, 
                 Handlers.WebLogic.buildResponse = responseBuilder }
              -- Handlers.WebLogic.buildResponse = \_ _ _ -> ResponseBuilder (error "sdf") (error "sdf") "sdf"}
-- run :: Port -> Application -> IO () --

  run 4221 (app handle)
  pure ()

