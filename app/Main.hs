module Main (main) where

import Control.Exception (bracket_)
import qualified Handlers.Logger
import qualified Handlers.WebLogic
import qualified Handlers.Base
import qualified Logger
import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, responseBuilder)
import Network.Wai (Request, Response, rawPathInfo, getRequestBodyChunk)
import Users
import Images
import Category
import qualified Base.MVar

-- old  type Application = Request -> ResourceT IO Response
--
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- type Respond = Response -> IO ResponseReceived


-- check user will do in another application later
--
app :: Handlers.WebLogic.Handle IO -> Application
app h req f =
  bracket_
   (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "Open app" )
   (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "Close app")
   (Handlers.WebLogic.doLogic h req >>= f)

main :: IO ()
main = do
  putStrLn "Welcome to WebServer"
  base <- Base.MVar.newBaseUser 
  baseImage <- Base.MVar.newBaseImage
  baseCategory <- Base.MVar.newBaseCategory
  limMessage <- loadWeb
  case limMessage of
   Left e -> putStrLn $ "fail decode config\n" <> e
   Right cfg -> do
    let logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = Logger.writeLog
            }
    let logBase =
          Handlers.Base.Handle
            { Handlers.Base.updateUser = Base.MVar.updateUser base,
              Handlers.Base.takeUsers = Base.MVar.takeUsers base, 
              Handlers.Base.findImage = Base.MVar.findImage baseImage, 
              Handlers.Base.takeCategories = Base.MVar.takeCategories baseCategory, 
              Handlers.Base.updateCategories = Base.MVar.updateCategories baseCategory, 
              Handlers.Base.logger = logHandle 
            }
    let handle = Handlers.WebLogic.Handle { 
                   Handlers.WebLogic.logger = logHandle, 
                   Handlers.WebLogic.buildResponse = responseBuilder,
                   Handlers.WebLogic.getBody = getRequestBodyChunk,
                   Handlers.WebLogic.base = logBase,
                   Handlers.WebLogic.paginate = limit cfg }
             
                -- Handlers.WebLogic.buildResponse = \_ _ _ -> ResponseBuilder (error "sdf") (error "sdf") "sdf"}
  -- run :: Port -> Application -> IO () --

    run 4221 (app handle)
    -- pure ()

