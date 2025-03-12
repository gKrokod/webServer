{-# LANGUAGE OverloadedStrings #-}

module Handlers.Router.News.GetSpec (spec) where

import Control.Monad.State (State, evalState, get)
import Data.Binary.Builder as BU (Builder)
import Data.Proxy (Proxy (..))
import Database.Data.FillTables (time4, user1test, user2test, user3test)
import qualified Handlers.Database.Base as DB
import qualified Handlers.Logger
import Handlers.Router (doLogic)
import Handlers.Web.Base (NewsOut (..))
import qualified Handlers.Web.Base as WB
import Network.HTTP.Types (status200)
import Network.Wai (defaultRequest, queryString, rawPathInfo, responseBuilder)
import Network.Wai.Internal (Response (..))
import Schema (ColumnType (..), SortOrder (..), User (..))
import Test.Hspec (Spec, it, shouldBe)
import Types (Content (..), Label (..), Login (..), Name (..), Title (..), URI_Image (..))
import Web.DTO.News (newsToWeb)

spec :: Spec
spec = do
  it "123" $ do head [23,14] `shouldBe` (23 :: Int)
--   let req = defaultrequest
--       req' = req {rawpathinfo = "/news", querystring = [("paginate", just "{\"offset\":0,\"limit\":10}")]}
--       loghandle =
--         handlers.logger.handle
--           { handlers.logger.levellogger = handlers.logger.debug,
--             handlers.logger.writelog = \_ -> pure ()
--           }
--       newsinbase =
--         [ mknewsout
--             { ntitle = mktitle "news1",
--               ntime = time4,
--               nauthor = mkname "user1test",
--               ncategories = map mklabel ["abstract", "man"],
--               ncontent = mkcontent "content1",
--               nimages = [mkuri_image "/images?id=1"],
--               nispublish = true
--             },
--           mknewsout
--             { ntitle = mktitle "news2",
--               ntime = time4,
--               nauthor = mkname "user2test",
--               ncategories = map mklabel ["abstract", "woman"],
--               ncontent = mkcontent "content2",
--               nimages = [],
--               nispublish = true
--             },
--           mknewsout
--             { ntitle = mktitle "news3",
--               ntime = time4,
--               nauthor = mkname "user3test",
--               ncategories = map mklabel ["abstract", "woman", "witch"],
--               ncontent = mkcontent "content3",
--               nimages = [],
--               nispublish = false
--             },
--           mknewsout
--             { ntitle = mktitle "news4",
--               ntime = time4,
--               nauthor = mkname "user1test",
--               ncategories = map mklabel ["abstract", "woman", "queen"],
--               ncontent = mkcontent "content4",
--               nimages = map mkuri_image ["/images?id=2", "/images?id=3"],
--               nispublish = true
--             }
--         ]
--
--       basehandle =
--         db.handle
--           { db.logger = loghandle,
--             db.sortcolumnnews = datanews,
--             db.sortordernews = descending,
--             db.findsubstring = nothing,
--             db.filtersnews = [],
--             db.pullallnews = \(db.mkoffset offset) (db.mklimit limit) _columntype _sortorder _find _filters -> get >>= pure . right . take limit . drop offset
--           }
--       webhandle =
--         wb.handle
--           { wb.logger = loghandle,
--             wb.base = basehandle,
--             WB.mkGoodResponse = testBuilder
--           } ::
--           WB.Handle (State [NewsOut])
--
--   it "All clients can get list of news" $ do
--     --
--     let baseHandle' = baseHandle
--         clientAdminUser1 = WB.Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1test)
--         clientAdminUser2 = WB.Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2test)
--         clientAdminUser3 = WB.Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3test)
--         clientAdminUser4 = WB.Client Nothing Nothing Nothing
--
--         webHandle1 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser1
--             }
--         webHandle2 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser2
--             }
--         webHandle3 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser3
--             }
--         webHandle4 =
--           webHandle
--             { WB.base = baseHandle',
--               WB.client = clientAdminUser4
--             }
--
--     evalState (doLogic webHandle1 req') newsInBase
--       `shouldBe` (testBuilder . newsToWeb $ newsInBase)
--     evalState (doLogic webHandle2 req') newsInBase
--       `shouldBe` (testBuilder . newsToWeb $ newsInBase)
--     evalState (doLogic webHandle3 req') newsInBase
--       `shouldBe` (testBuilder . newsToWeb $ newsInBase)
--     evalState (doLogic webHandle4 req') newsInBase
--       `shouldBe` (testBuilder . newsToWeb $ newsInBase)
--
-- testBuilder :: Builder -> Response
-- testBuilder = responseBuilder status200 []
--
-- instance Show Response where
--   show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
--   show _ = "Response"
--
-- instance Eq Response where
--   (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
--   (==) _ _ = undefined
