module Main (main) where
--haha
import Config (loadConfig, ConfigDataBase, connectionString, whenMakeTables, cLimitData)
import Scheme
-- import Database.Persist.Postgresql (ConnectionString)
import qualified Data.ByteString.Lazy as L 
import qualified Handlers.Logger
import Handlers.Logger (Log(Debug))
import qualified Handlers.Base
import qualified Base.Base as BB 
import qualified Base.Crypto
import qualified Handlers.WebLogic
import qualified Web.WebLogic as WW 

import qualified Logger
import Database.Persist.Postgresql  (keyValueEntityToJSON, ConnectionString, runMigration, entityIdToJSON)
import Data.Time (getCurrentTime)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Network.Wai.Handler.Warp (run)
import Network.Wai (Application, responseBuilder, ResponseReceived, Response, Request)
-- import Network.Wai (Request, Response, rawPathInfo, getRequestBodyChunk)
import Control.Exception (bracket_)

type Respond = Response -> IO ResponseReceived

main :: IO ()
main = do 
  Logger.writeLog "HEEEEREEE WE STAAAART MAINNNN"
  config <- loadConfig
-- make Tables and Fill its if need
  whenMakeTables config $ Logger.writeLog "MAKE AND FILL TABLES" 
                          >> BB.makeAndFillTables (connectionString config)
  serverSetup <- makeSetup config
  -- run 4221 (app serverSetup ) 
  run 4221 (autorization serverSetup app) 


-- old  type Application = Request -> ResourceT IO Response
--
-- last type Application = Request -> (Response -> IO ResponseReceived) -> IO ResponseReceived -- passing style?
-- type Application :: Request -> Respond -> IO ResponseReceived
-- check user will do in another application later
--
type ServerSetup m = Handlers.WebLogic.Handle m 
--
app :: ServerSetup IO -> Application
app h req f =
  bracket_
   (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "Open app" )
   (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "Close app")
   (Handlers.WebLogic.doLogic h req >>= f)

autorization :: ServerSetup IO -> (ServerSetup IO -> Application) -> Application
-- autorization :: ServerSetup IO -> (ServerSetup IO -> Application) -> Request -> Respond -> IO ResponseReceived
autorization h nextApp req respond = 
  bracket_
   (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "OpenAuth app" )
   (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "CloseAuth app")
   -- (undefined)
   (do
      check <- Handlers.WebLogic.doAutorization h req
      case check of 
       Left fail -> respond fail
       Right h' -> nextApp h' req respond )

makeSetup :: ConfigDataBase -> IO (ServerSetup IO) 
makeSetup cfg = do
  Logger.writeLog "MAKE SETUP"
  let pginfo = connectionString cfg
  t <- getCurrentTime
  Logger.writeLog ( T.pack $ show t )
  let logHandle = Handlers.Logger.Handle
        { Handlers.Logger.levelLogger = Debug,
          Handlers.Logger.writeLog = Logger.writeLog
        }
  let baseHandle = Handlers.Base.Handle
        { Handlers.Base.logger = logHandle,
          Handlers.Base.putUser = BB.putUser pginfo,
          Handlers.Base.findUserByLogin = BB.findUserByLogin pginfo,
          Handlers.Base.getTime = getCurrentTime,
          Handlers.Base.makeHashPassword = Base.Crypto.makeHashPassword,
          Handlers.Base.validPassword = BB.validPassword pginfo,
-- default setup
          Handlers.Base.userOffset = 0,
          Handlers.Base.userLimit = maxBound,
          Handlers.Base.sortColumnNews = DataNews,
          Handlers.Base.sortOrderNews = Descending,
          Handlers.Base.findSubString = Nothing,
          Handlers.Base.filtersNews = [],
-- default *
          Handlers.Base.pullAllUsers = BB.pullAllUsers pginfo (cLimitData cfg),
          Handlers.Base.findCategoryByLabel = BB.findCategoryByLabel pginfo,
          Handlers.Base.putCategory = BB.putCategory pginfo,
          Handlers.Base.editCategory = BB.editCategory pginfo,
          Handlers.Base.pullAllCategories = BB.pullAllCategories pginfo (cLimitData cfg),
          Handlers.Base.pullImage = BB.pullImage pginfo,
          Handlers.Base.putNews = BB.putNews pginfo,
          Handlers.Base.findNewsByTitle = BB.findNewsByTitle pginfo,
          Handlers.Base.pullAllNews = BB.pullAllNews pginfo (cLimitData cfg),
          Handlers.Base.editNews = BB.editNews pginfo
      }
  let handle = Handlers.WebLogic.Handle { 
          Handlers.WebLogic.logger = logHandle, 
          Handlers.WebLogic.base = baseHandle,
          -- Handlers.WebLogic.privilege = Anonymous,
          Handlers.WebLogic.client = Handlers.WebLogic.Client Nothing Nothing,
          Handlers.WebLogic.response404 = WW.response404,
          Handlers.WebLogic.response200 = WW.response200,
          Handlers.WebLogic.mkGoodResponse = WW.mkGoodResponse,
          Handlers.WebLogic.mkResponseForImage = WW.mkResponseForImage,
          -- Handlers.WebLogic.getQueryString = WW.getQueryString,
          Handlers.WebLogic.getBody = WW.getBody}
  pure handle
