module Main (main) where

import Config (ConfigDataBase, cLimitData, cLogLvl, connectionString, loadConfig, whenMakeTables)
import Control.Exception (bracket_)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Database.Api as DA
import qualified Database.Crypto
import qualified Handlers.Database.Base
import Handlers.Logger (Log (Debug))
import qualified Handlers.Logger
import Handlers.Router (doAuthorization, doLogic)
import qualified Handlers.Web.Base
import qualified Logger
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Scheme (ColumnType (..), SortOrder (..))
import qualified Web.WebLogic as WL

main :: IO ()
main = do
  Logger.writeLog "Welcome!"
  config <- loadConfig
  -- make Tables and Fill its if need
  whenMakeTables config $
    Logger.writeLog "Create and fill tables in the database"
      >> DA.makeAndFillTables (connectionString config)
  serverSetup <- makeSetup config
  run 4221 $ authorization serverSetup app

type ServerSetup m = Handlers.Web.Base.Handle m

app :: ServerSetup IO -> Application
app h req f =
  bracket_
    (Handlers.Logger.logMessage (Handlers.Web.Base.logger h) Handlers.Logger.Debug "Open app")
    (Handlers.Logger.logMessage (Handlers.Web.Base.logger h) Handlers.Logger.Debug "Close app")
    (doLogic h req >>= f)

authorization :: ServerSetup IO -> (ServerSetup IO -> Application) -> Application
authorization h nextApp req respond =
  bracket_
    (Handlers.Logger.logMessage (Handlers.Web.Base.logger h) Handlers.Logger.Debug "OpenAuth app")
    (Handlers.Logger.logMessage (Handlers.Web.Base.logger h) Handlers.Logger.Debug "CloseAuth app")
    ( do
        check <- doAuthorization h req
        case check of
          Left fail' -> respond fail'
          Right h' -> nextApp h' req respond
    )

makeSetup :: ConfigDataBase -> IO (ServerSetup IO)
makeSetup cfg = do
  Logger.writeLog "Setting up the server"
  let pginfo = connectionString cfg
  t <- getCurrentTime
  Logger.writeLog ("Launch time: " <> T.pack (show t))
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = cLogLvl cfg,
            Handlers.Logger.writeLog = Logger.writeLog
          }
      baseHandle =
        Handlers.Database.Base.Handle
          { Handlers.Database.Base.logger = logHandle,
            Handlers.Database.Base.putUser = DA.putUser pginfo,
            Handlers.Database.Base.findUserByLogin = DA.findUserByLogin pginfo,
            Handlers.Database.Base.getTime = getCurrentTime,
            Handlers.Database.Base.makeHashPassword = Database.Crypto.makeHashPassword,
            Handlers.Database.Base.validPassword = DA.validPassword pginfo,
            Handlers.Database.Base.validCopyRight = DA.validCopyRight pginfo,
            -- default setup
            Handlers.Database.Base.userOffset = 0,
            Handlers.Database.Base.userLimit = maxBound,
            Handlers.Database.Base.sortColumnNews = DataNews,
            Handlers.Database.Base.sortOrderNews = Descending,
            Handlers.Database.Base.findSubString = Nothing,
            Handlers.Database.Base.filtersNews = [],
            -- default *
            Handlers.Database.Base.pullAllUsers = DA.pullAllUsers pginfo (cLimitData cfg),
            Handlers.Database.Base.findCategoryByLabel = DA.findCategoryByLabel pginfo,
            Handlers.Database.Base.putCategory = DA.putCategory pginfo,
            Handlers.Database.Base.editCategory = DA.editCategory pginfo,
            Handlers.Database.Base.pullAllCategories = DA.pullAllCategories pginfo (cLimitData cfg),
            Handlers.Database.Base.pullImage = DA.pullImage pginfo,
            Handlers.Database.Base.putNews = DA.putNews pginfo,
            Handlers.Database.Base.findNewsByTitle = DA.findNewsByTitle pginfo,
            Handlers.Database.Base.pullAllNews = DA.pullAllNews pginfo (cLimitData cfg),
            Handlers.Database.Base.editNews = DA.editNews pginfo
          }
      handle =
        Handlers.Web.Base.Handle
          { Handlers.Web.Base.logger = logHandle,
            Handlers.Web.Base.base = baseHandle,
            Handlers.Web.Base.client = Handlers.Web.Base.Client Nothing Nothing Nothing,
            Handlers.Web.Base.response404 = WL.response404,
            Handlers.Web.Base.response200 = WL.response200,
            Handlers.Web.Base.mkGoodResponse = WL.mkGoodResponse,
            Handlers.Web.Base.mkResponseForImage = WL.mkResponseForImage,
            Handlers.Web.Base.response404WithImage = WL.response404WithImage,
            Handlers.Web.Base.getBody = WL.getBody
          }
  pure handle
