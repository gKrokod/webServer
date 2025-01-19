module Main (main) where

import Config (ConfigDataBase (..), connectionString, loadConfig)
import Control.Exception (bracket_)
import Data.Time (getCurrentTime)
import qualified Database.Api as DA
import qualified Handlers.Database.Base
import qualified Handlers.Database.User
import qualified Handlers.Web.User
import qualified Handlers.Database.Image
import qualified Handlers.Web.Image
import Handlers.Logger (Log (Info), logMessage)
import qualified Handlers.Logger
import Handlers.Router (doAuthorization, doLogic)
import qualified Handlers.Web.Base
import qualified Logger
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Schema (ColumnType (..), SortOrder (..))
import qualified Web.Utils as WU

main :: IO ()
main = do
  config <- loadConfig

  DA.migrationEngine (connectionString config)

  serverSetup <- makeSetup config

  run (cPortServer config) $ authorization serverSetup app

type ServerSetup m = Handlers.Web.Base.Handle m

app :: ServerSetup IO -> Application
app h req f =
  bracket_
    (logMessage (Handlers.Web.Base.logger h) Info "Open app")
    (logMessage (Handlers.Web.Base.logger h) Info "Close app")
    (doLogic h req >>= f)

authorization :: ServerSetup IO -> (ServerSetup IO -> Application) -> Application
authorization h nextApp req respond = do
  check <- doAuthorization h req
  case check of
    Left fail' -> respond fail'
    Right h' -> nextApp h' req respond

makeSetup :: ConfigDataBase -> IO (ServerSetup IO)
makeSetup cfg = do
  let pginfo = connectionString cfg
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = cLogLvl cfg,
            Handlers.Logger.writeLog = Logger.writeLog
          }
      baseImageHandle =
        Handlers.Database.Image.Handle
          { Handlers.Database.Image.logger = logHandle,
            Handlers.Database.Image.pullImage = DA.pullImage pginfo
          }
      imageHandle = Handlers.Web.Image.Handle {
          Handlers.Web.Image.logger = logHandle,
          Handlers.Web.Image.base = baseImageHandle,
          Handlers.Web.Image.response404 = WU.response404,
          Handlers.Web.Image.response400 = WU.response400,
          Handlers.Web.Image.response500 = WU.response500,
          Handlers.Web.Image.mkGoodResponse = WU.mkGoodResponse,
          Handlers.Web.Image.mkResponseForImage = WU.mkResponseForImage
        }
      baseUserHandle =
        Handlers.Database.User.Handle
          { Handlers.Database.User.logger = logHandle,
            Handlers.Database.User.userOffset = 0,
            Handlers.Database.User.userLimit = maxBound,
            Handlers.Database.User.getTime = getCurrentTime,
            Handlers.Database.User.makeHashPassword = DA.makeHashPassword,
            Handlers.Database.User.pullAllUsers = DA.pullAllUsers pginfo (cLimitData cfg),
            Handlers.Database.User.findUserByLogin = DA.findUserByLogin pginfo,
            Handlers.Database.User.putUser = DA.putUser pginfo
          }
      userHandle = Handlers.Web.User.Handle {
          Handlers.Web.User.logger = logHandle,
          Handlers.Web.User.base = baseUserHandle,
          Handlers.Web.User.response400 = WU.response400,
          Handlers.Web.User.response500 = WU.response500,
          Handlers.Web.User.response200 = WU.response200,
          Handlers.Web.User.response404 = WU.response404,
          Handlers.Web.User.mkGoodResponse = WU.mkGoodResponse,
          Handlers.Web.User.getBody = WU.getBody
        }
      baseHandle =
        Handlers.Database.Base.Handle
          { Handlers.Database.Base.logger = logHandle,
            -- Handlers.Database.Base.putUser = DA.putUser pginfo,
            Handlers.Database.Base.findUserByLogin = DA.findUserByLogin pginfo, --нужен для авторизации. там проверка
            Handlers.Database.Base.getTime = getCurrentTime,
            Handlers.Database.Base.makeHashPassword = DA.makeHashPassword,
            Handlers.Database.Base.validPassword = DA.validPassword pginfo,
            Handlers.Database.Base.validCopyRight = DA.validCopyRight pginfo,
            Handlers.Database.Base.userOffset = 0,
            Handlers.Database.Base.userLimit = maxBound,
            Handlers.Database.Base.sortColumnNews = DataNews,
            Handlers.Database.Base.sortOrderNews = Descending,
            Handlers.Database.Base.findSubString = Nothing,
            Handlers.Database.Base.filtersNews = [],
            -- Handlers.Database.Base.pullAllUsers = DA.pullAllUsers pginfo (cLimitData cfg),
            Handlers.Database.Base.findCategoryByLabel = DA.findCategoryByLabel pginfo,
            Handlers.Database.Base.putCategory = DA.putCategory pginfo,
            Handlers.Database.Base.editCategory = DA.editCategory pginfo,
            Handlers.Database.Base.pullAllCategories = DA.pullAllCategories pginfo (cLimitData cfg),
            -- Handlers.Database.Base.pullImage = DA.pullImage pginfo, -- *
            Handlers.Database.Base.putNews = DA.putNews pginfo,
            Handlers.Database.Base.findNewsByTitle = DA.findNewsByTitle pginfo,
            Handlers.Database.Base.pullAllNews = DA.pullAllNews pginfo (cLimitData cfg),
            Handlers.Database.Base.editNews = DA.editNews pginfo
          }
      handle =
        Handlers.Web.Base.Handle
          {  
            Handlers.Web.Base.connectionString = pginfo, 
            Handlers.Web.Base.logger = logHandle,
            Handlers.Web.Base.base = baseHandle,
            Handlers.Web.Base.client =
              Handlers.Web.Base.Client
                { Handlers.Web.Base.clientAdminToken = Nothing,
                  Handlers.Web.Base.clientPublisherToken = Nothing,
                  Handlers.Web.Base.author = Nothing
                },
            Handlers.Web.Base.response404 = WU.response404,
            Handlers.Web.Base.response200 = WU.response200,
            Handlers.Web.Base.response403 = WU.response403,
            Handlers.Web.Base.response400 = WU.response400,
            Handlers.Web.Base.response500 = WU.response500,
            Handlers.Web.Base.mkGoodResponse = WU.mkGoodResponse,
            -- Handlers.Web.Base.mkResponseForImage = WU.mkResponseForImage,
            -- Handlers.Web.Base.response404WithImage = WU.response404WithImage,
            Handlers.Web.Base.getBody = WU.getBody,
            Handlers.Web.Base.user = userHandle,
            Handlers.Web.Base.image = imageHandle
          }
  pure handle
