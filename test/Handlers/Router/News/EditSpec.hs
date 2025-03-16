{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.News.EditSpec (spec) where

import Control.Monad.State (State, evalState, gets)
import Data.Binary.Builder as BU (Builder)
import Database.Data.FillTables (cat1, news1, news2, news3, news4, time4, user1test)
import qualified Handlers.Database.Auth
import Handlers.Database.Base (Success (..))
import qualified Handlers.Database.News
import qualified Handlers.Logger
import Handlers.Web.Base (NewsEditInternal (..))
import qualified Handlers.Web.Base as WB
import Handlers.Web.News (Handle (..))
import Handlers.Web.News.Update (updateNews)
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (ColumnType (..), News (..), SortOrder (..))
import Test.Hspec (Spec, it, shouldBe, shouldNotBe)
import Types (Title (..))
import qualified Web.Utils as WU

spec :: Spec
spec = do
  let req = defaultRequest
      req' = req {rawPathInfo = "/news/edit"}
      bodyReq = "{\"title\":\"News 1 about Witch from user 1\",\"newTitle\":\"Edit News1\",\"newIsPublish\":true,\"newLogin\":\"login3\",\"newLabel\":\"Man\",\"newContent\":\"New Content\",\"images\":[{\"imageHeader\":\"edit image\",\"imageBase64\":\"edit kartinka for news sh\"}]}"
      newsInBase = [news1, news2, news3, news4]
      logHandle =
        Handlers.Logger.Handle
          { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
            Handlers.Logger.writeLog = \_ -> pure ()
          }
      authHandle =
        Handlers.Database.Auth.Handle
          { Handlers.Database.Auth.logger = logHandle,
            Handlers.Database.Auth.findUserByLogin = \_ -> pure $ Right Nothing,
            Handlers.Database.Auth.validPassword = \_ _ -> pure $ Right True,
            Handlers.Database.Auth.client = Handlers.Database.Auth.Client Nothing Nothing Nothing,
            Handlers.Database.Auth.validCopyRight = \_ _ -> pure $ Right True
          }
      baseNewsHandle =
        Handlers.Database.News.Handle
          { Handlers.Database.News.logger = logHandle,
            Handlers.Database.News.userOffset = 0,
            Handlers.Database.News.userLimit = maxBound,
            Handlers.Database.News.getTime = pure time4,
            Handlers.Database.News.findUserByLogin = const (pure $ Right $ Just user1test),
            Handlers.Database.News.sortColumnNews = DataNews,
            Handlers.Database.News.sortOrderNews = Descending,
            Handlers.Database.News.findSubString = Nothing,
            Handlers.Database.News.filtersNews = [],
            Handlers.Database.News.findCategoryByLabel = const (pure . Right $ Just cat1),
            Handlers.Database.News.putNews = \(WB.NewsInternal _title _login _label _content _images _isPublish) _time -> pure $ Right Put,
            Handlers.Database.News.findNewsByTitle =
              \title -> do
                titles <- gets (map (MkTitle . newsTitle))
                pure $
                  Right $
                    if title `elem` titles
                      then Just undefined
                      else Nothing,
            Handlers.Database.News.pullAllNews = \_ _ _columntype _sortorder _find _filters -> pure $ Right [],
            Handlers.Database.News.editNews = \_title _time (NewsEditInternal _mbtitle _mblogin _mblabel _mbcontent _image _mbp) -> pure $ Right Change
          }

      newsHandle =
        Handlers.Web.News.Handle
          { Handlers.Web.News.logger = logHandle,
            Handlers.Web.News.base = baseNewsHandle,
            Handlers.Web.News.auth = authHandle,
            Handlers.Web.News.client = Handlers.Database.Auth.Client Nothing Nothing Nothing,
            Handlers.Web.News.response400 = WU.response400,
            Handlers.Web.News.response500 = WU.response500,
            Handlers.Web.News.response200 = WU.response200,
            Handlers.Web.News.response404 = WU.response404,
            Handlers.Web.News.response403 = WU.response403,
            Handlers.Web.News.mkGoodResponse = testBuilder,
            Handlers.Web.News.getBody = const . pure $ bodyReq
          } ::
          Handlers.Web.News.Handle (State [News])
  --
  it "Author can edit news" $ do
    let authHandle' = authHandle {Handlers.Database.Auth.validCopyRight = \_login _title -> pure $ Right True}
        newsHandle' =
          newsHandle
            { Handlers.Web.News.auth = authHandle'
            }
    evalState (updateNews (error "Author") newsHandle' req') newsInBase
      `shouldBe` test200

  it "Non-author can't edit news" $ do
    let authHandle' = authHandle {Handlers.Database.Auth.validCopyRight = \_login _title -> pure $ Right False}
        newsHandle' =
          newsHandle
            { Handlers.Web.News.auth = authHandle'
            }
    evalState (updateNews (error "No-Author") newsHandle' req') newsInBase
      `shouldNotBe` test200

test200 :: Response
test200 = responseBuilder status200 [] "All ok. status 200\n"

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = "Response"

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []
