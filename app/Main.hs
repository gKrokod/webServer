module Main (main) where

import qualified Base.Base as BB
import qualified Base.Crypto
import Config (ConfigDataBase, cLimitData, cLogLvl, connectionString, loadConfig, whenMakeTables)
import Control.Exception (bracket_)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Handlers.Base.Base
import Handlers.Logger (Log (Debug))
import qualified Handlers.Logger
import Handlers.Router (doAuthorization, doLogic)
import qualified Handlers.Web.Web
import qualified Logger
import Network.Wai (Application)
import Network.Wai.Handler.Warp (run)
import Scheme (ColumnType (..), SortOrder (..))
import qualified Web.WebLogic as WW

main :: IO ()
main = do
  Logger.writeLog "Welcome!"
  config <- loadConfig
  -- make Tables and Fill its if need
  whenMakeTables config $
    Logger.writeLog "Create and fill tables in the database"
      >> BB.makeAndFillTables (connectionString config)
  serverSetup <- makeSetup config
  run 4221 $ authorization serverSetup app

type ServerSetup m = Handlers.Web.Web.Handle m

app :: ServerSetup IO -> Application
app h req f =
  bracket_
    (Handlers.Logger.logMessage (Handlers.Web.Web.logger h) Handlers.Logger.Debug "Open app")
    (Handlers.Logger.logMessage (Handlers.Web.Web.logger h) Handlers.Logger.Debug "Close app")
    (doLogic h req >>= f)

authorization :: ServerSetup IO -> (ServerSetup IO -> Application) -> Application
authorization h nextApp req respond =
  bracket_
    (Handlers.Logger.logMessage (Handlers.Web.Web.logger h) Handlers.Logger.Debug "OpenAuth app")
    (Handlers.Logger.logMessage (Handlers.Web.Web.logger h) Handlers.Logger.Debug "CloseAuth app")
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
        Handlers.Base.Base.Handle
          { Handlers.Base.Base.logger = logHandle,
            Handlers.Base.Base.putUser = BB.putUser pginfo,
            Handlers.Base.Base.findUserByLogin = BB.findUserByLogin pginfo,
            Handlers.Base.Base.getTime = getCurrentTime,
            Handlers.Base.Base.makeHashPassword = Base.Crypto.makeHashPassword,
            Handlers.Base.Base.validPassword = BB.validPassword pginfo,
            Handlers.Base.Base.validCopyRight = BB.validCopyRight pginfo,
            -- default setup
            Handlers.Base.Base.userOffset = 0,
            Handlers.Base.Base.userLimit = maxBound,
            Handlers.Base.Base.sortColumnNews = DataNews,
            Handlers.Base.Base.sortOrderNews = Descending,
            Handlers.Base.Base.findSubString = Nothing,
            Handlers.Base.Base.filtersNews = [],
            -- default *
            Handlers.Base.Base.pullAllUsers = BB.pullAllUsers pginfo (cLimitData cfg),
            Handlers.Base.Base.findCategoryByLabel = BB.findCategoryByLabel pginfo,
            Handlers.Base.Base.putCategory = BB.putCategory pginfo,
            Handlers.Base.Base.editCategory = BB.editCategory pginfo,
            Handlers.Base.Base.pullAllCategories = BB.pullAllCategories pginfo (cLimitData cfg),
            Handlers.Base.Base.pullImage = BB.pullImage pginfo,
            Handlers.Base.Base.putNews = BB.putNews pginfo,
            Handlers.Base.Base.findNewsByTitle = BB.findNewsByTitle pginfo,
            Handlers.Base.Base.pullAllNews = BB.pullAllNews pginfo (cLimitData cfg),
            Handlers.Base.Base.editNews = BB.editNews pginfo
          }
      handle =
        Handlers.Web.Web.Handle
          { Handlers.Web.Web.logger = logHandle,
            Handlers.Web.Web.base = baseHandle,
            Handlers.Web.Web.client = Handlers.Web.Web.Client Nothing Nothing Nothing,
            Handlers.Web.Web.response404 = WW.response404,
            Handlers.Web.Web.response200 = WW.response200,
            Handlers.Web.Web.mkGoodResponse = WW.mkGoodResponse,
            Handlers.Web.Web.mkResponseForImage = WW.mkResponseForImage,
            Handlers.Web.Web.response404WithImage = WW.response404WithImage,
            Handlers.Web.Web.getBody = WW.getBody
          }
  pure handle
