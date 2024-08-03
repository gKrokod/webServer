module Main (main) where

import qualified Base.Base as BB
import qualified Base.Crypto
import Config (ConfigDataBase, cLogLvl, cLimitData, connectionString, loadConfig, whenMakeTables)
import Control.Exception (bracket_)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import qualified Handlers.Base
import Handlers.Logger (Log (Debug))
import qualified Handlers.Logger
import qualified Handlers.WebLogic
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

type ServerSetup m = Handlers.WebLogic.Handle m

app :: ServerSetup IO -> Application
app h req f =
  bracket_
    (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "Open app")
    (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "Close app")
    (Handlers.WebLogic.doLogic h req >>= f)

authorization :: ServerSetup IO -> (ServerSetup IO -> Application) -> Application
authorization h nextApp req respond =
  bracket_
    (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "OpenAuth app")
    (Handlers.Logger.logMessage (Handlers.WebLogic.logger h) Handlers.Logger.Debug "CloseAuth app")
    ( do
        check <- Handlers.WebLogic.doAuthorization h req
        case check of
          Left fail' -> respond fail'
          Right h' -> nextApp h' req respond
    )

makeSetup :: ConfigDataBase -> IO (ServerSetup IO)
makeSetup cfg = do
  Logger.writeLog "Setting up the server"
  let pginfo = connectionString cfg
  t <- getCurrentTime
  Logger.writeLog ("Launch time: " <> (T.pack $ show t))
  let logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = cLogLvl cfg,
            Handlers.Logger.writeLog = Logger.writeLog
          }
      baseHandle =
        Handlers.Base.Handle
          { Handlers.Base.logger = logHandle,
            Handlers.Base.putUser = BB.putUser pginfo,
            Handlers.Base.findUserByLogin = BB.findUserByLogin pginfo,
            Handlers.Base.getTime = getCurrentTime,
            Handlers.Base.makeHashPassword = Base.Crypto.makeHashPassword,
            Handlers.Base.validPassword = BB.validPassword pginfo,
            Handlers.Base.validCopyRight = BB.validCopyRight pginfo,
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
      handle =
        Handlers.WebLogic.Handle
          { Handlers.WebLogic.logger = logHandle,
            Handlers.WebLogic.base = baseHandle,
            Handlers.WebLogic.client = Handlers.WebLogic.Client Nothing Nothing Nothing,
            Handlers.WebLogic.response404 = WW.response404,
            Handlers.WebLogic.response200 = WW.response200,
            Handlers.WebLogic.mkGoodResponse = WW.mkGoodResponse,
            Handlers.WebLogic.mkResponseForImage = WW.mkResponseForImage,
            Handlers.WebLogic.response404WithImage = WW.response404WithImage,
            Handlers.WebLogic.getBody = WW.getBody
          }
  pure handle
