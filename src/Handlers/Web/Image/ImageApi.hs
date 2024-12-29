module Handlers.Web.Image.ImageApi (endPointImages) where

import qualified Handlers.Logger
import qualified Handlers.Database.Base
import Handlers.Web.Base (Handle (..))
import Handlers.Web.Image.Get (existingImages)
import Network.Wai (Request, Response, rawPathInfo)
import Data.Text (Text)
import Schema (Image (..))

import qualified Database.Api as DA
            -- Handlers.Database.Base.pullImage = DA.pullImage pginfo,

endPointImages :: (Monad m) => Handle m -> Request -> m Response
endPointImages h req = do
  case rawPathInfo req of
    "/images" -> existingImages h req
    _ -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"
      pure $ response404 h

data HandleImage m = HandleImage
  { 
    response400 :: Text -> Response,
    response500 :: Response,
    mkResponseForImage :: Image -> Response,
    logHandle :: Handlers.Logger.Handle m,
    baseHandle :: Handlers.Database.Base.Handle m
    -- pullImage :: NumberImage -> m (Either SomeException (Maybe Image))
  }        

-- getImage :: (Monad m) => Handle m -> NumberImage -> m (Either T.Text Image)
-- getImage h uid = do
--   let logHandle = logger h
--   images <- pullImage h uid
--   case images of
--     Left e -> do
--       logMessage logHandle Handlers.Logger.Error "function pullImage fail"
--       pure . Left . T.pack . show $ e
--     Right Nothing -> do
--       pure $ Left "Image was not found in database"
--     Right (Just image) -> do
--       pure $ Right image

      -- handle =
      --   Handlers.Web.Base.Handle
      --     { Handlers.Web.Base.logger = logHandle,
      --       Handlers.Web.Base.base = baseHandle,
      --       Handlers.Web.Base.client =
      --         Handlers.Web.Base.Client
      --           { Handlers.Web.Base.clientAdminToken = Nothing,
      --             Handlers.Web.Base.clientPublisherToken = Nothing,
      --             Handlers.Web.Base.author = Nothing
      --           },
      --       Handlers.Web.Base.response404 = WU.response404,
      --       Handlers.Web.Base.response200 = WU.response200,
      --       Handlers.Web.Base.response403 = WU.response403,
      --       Handlers.Web.Base.response400 = WU.response400,
      --       Handlers.Web.Base.response500 = WU.response500,
      --       Handlers.Web.Base.mkGoodResponse = WU.mkGoodResponse,
      --       Handlers.Web.Base.mkResponseForImage = WU.mkResponseForImage,
      --       Handlers.Web.Base.response404WithImage = WU.response404WithImage,
      --       Handlers.Web.Base.getBody = WU.getBody
      --     }
--   { levelLogger :: Log,
--     writeLog :: T.Text -> m ()
  
  -- let logHandle =
  --       Handlers.Logger.Handle
  --         { Handlers.Logger.levelLogger = cLogLvl cfg,
  --           Handlers.Logger.writeLog = Logger.writeLog
  --         }
--
--Web data Handle m = Handle
--   { logger :: Handlers.Logger.Handle m,
--     base :: Handlers.Database.Base.Handle m,
--     client :: Client,
--     getBody :: Request -> m B.ByteString,
--     response404 :: Response,
--     response200 :: Response,
--     response403 :: Response,
--  ++ response400 :: Text -> Response,
--  ++ response500 :: Response,
--     mkGoodResponse :: Builder -> Response,
--  ++ mkResponseForImage :: Image -> Response,
--     response404WithImage :: Response
--   }
--Base data Handle m = Handle
--   { logger :: Handlers.Logger.Handle m,
--     userOffset :: Int,
--     userLimit :: Int,
--     sortColumnNews :: ColumnType,
--     sortOrderNews :: SortOrder,
--     findSubString :: Maybe Find,
--     filtersNews :: [FilterItem],
--     getTime :: m UTCTime,
--     makeHashPassword :: PasswordUser -> UTCTime -> HashPasswordUser,
--     validPassword :: Login -> PasswordUser -> m (Either SomeException Bool),
--     validCopyRight :: Login -> Title -> m (Either SomeException Bool),
--     pullAllUsers :: Offset -> Limit -> m (Either SomeException [User]),
--     pullAllNews :: Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> m (Either SomeException [NewsOut]),
--     pullAllCategories :: Offset -> Limit -> m (Either SomeException [Category]),
--     pullImage :: NumberImage -> m (Either SomeException (Maybe Image)),
--     findUserByLogin :: Login -> m (Either SomeException (Maybe User)),
--     findCategoryByLabel :: Label -> m (Either SomeException (Maybe Category)),
--     findNewsByTitle :: Title -> m (Either SomeException (Maybe News)),
--     putUser :: UserInternal -> UTCTime -> m (Either SomeException Success),
--     putCategory :: CategoryInternal -> m (Either SomeException Success),
--     putNews :: NewsInternal -> UTCTime -> m (Either SomeException Success),
--     editNews :: Title -> UTCTime -> NewsEditInternal -> m (Either SomeException Success),
--     editCategory :: Label -> CategoryInternal -> m (Either SomeException Success)
--   }
-- data Handle m = Handle
--   { levelLogger :: Log,
--     writeLog :: T.Text -> m ()
--   }
