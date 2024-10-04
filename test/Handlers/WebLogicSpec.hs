{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}

module Handlers.WebLogicSpec (spec) where

import Base.FillTables (cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9, image1, image2, image3, news1, news2, news3, news4, user1, user2, user3)
import Base.LocalTime (localtimeTemplate)
import Control.Monad.Identity (Identity, runIdentity)
import Control.Monad.State (State, evalState, get, gets)
import Data.Binary.Builder as BU (Builder, fromByteString)
import Data.ByteString.Base64 as B64
import Data.Either (isLeft)
import Data.Maybe (listToMaybe, mapMaybe)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import qualified Data.Text.Encoding as E
import Data.Time (Day (..), UTCTime (..), fromGregorian)
import qualified Handlers.Base
import qualified Handlers.Logger
import Handlers.WebLogic
import Network.HTTP.Types (hContentType, notFound404, status200)
import Network.Wai (Request, defaultRequest, getRequestBodyChunk, queryString, rawPathInfo, rawQueryString, requestHeaders, responseBuilder)
import Network.Wai.Internal (Response (..))
import Scheme (Category (..), ColumnType (..), FilterItem (..), Find (..), Image (..), News (..), SortOrder (..), User (..))
import Test.Hspec (Spec, describe, it, shouldBe, shouldNotBe)
import Types (CategoryInternal (..), Content (..), Label (..), Login (..), Name (..), NewsEditInternal (..), NewsInternal (..), NewsOut (..), NumberImage (..), Title (..), URI_Image (..), UserInternal (..))
import Web.WebType (categoryToWeb, newsToWeb, userToWeb)

--

spec :: Spec
spec = do
  describe "Autorization:" $ do
    -- user1 admin noPublisher
    -- user2 admin publisher
    -- user3 noAdmin publisher
    -- unknown user noAdmin noPublisher
    --
    let req = defaultRequest

        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

        usersInBase = [user1, user2, user3]
        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.findUserByLogin = \(MkLogin login) ->
                gets
                  ( Right
                      . listToMaybe
                      . mapMaybe
                        ( \user@(User _ l _ _ _ _) ->
                            if l == login then Just user else Nothing
                        )
                  ),
              Handlers.Base.validPassword = \login password -> pure $ Right True
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              response404 = test404
            } ::
            Handle (State [User])

    it "Unknown user does not received privileges" $ do
      let req' = req {requestHeaders = []}
          webHandle' = webHandle

      client <$> evalState (doAuthorization webHandle' req') usersInBase
        `shouldBe` Right (Client Nothing Nothing Nothing)

    it "A user with a valid password receives their privileges" $ do
      let req1 = req {requestHeaders = [("Authorization", "Basic bG9naW4xOnFwYXNzMQ==")]} -- user1
          req2 = req {requestHeaders = [("Authorization", "Basic bG9naW4yOnFwYXNzMg==")]} -- user2
          req3 = req {requestHeaders = [("Authorization", "Basic bG9naW4zOnFwYXNzMw==")]} -- user3
          webHandle' = webHandle

      client <$> evalState (doAuthorization webHandle' req1) usersInBase
        `shouldBe` Right (Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)) -- user1
      client <$> evalState (doAuthorization webHandle' req2) usersInBase
        `shouldBe` Right (Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2)) -- user2
      client <$> evalState (doAuthorization webHandle' req3) usersInBase
        `shouldBe` Right (Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)) -- user3
    it "A user with an invalid password does not receive his privileges" $ do
      let req1 = req {requestHeaders = [("Authorization", "Basic bG9naW4xOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user1
          req2 = req {requestHeaders = [("Authorization", "Basic bG9naW4yOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user2
          req3 = req {requestHeaders = [("Authorization", "Basic bG9naW4zOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user3
          baseHandle' = baseHandle {Handlers.Base.validPassword = \login password -> pure $ Right False}
          webHandle' = webHandle {base = baseHandle'}

      client <$> evalState (doAuthorization webHandle' req1) usersInBase
        `shouldNotBe` Right (Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)) -- user1
      client <$> evalState (doAuthorization webHandle' req2) usersInBase
        `shouldNotBe` Right (Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2)) -- user2
      client <$> evalState (doAuthorization webHandle' req3) usersInBase
        `shouldNotBe` Right (Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)) -- user3
  describe "EndPoint: /users" $ do
    let req = defaultRequest
        req' = req {rawPathInfo = "/users", queryString = [("panigate", Just "{\"offset\":0,\"limit\":7}")]}

        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

        usersInBase = [user1, user2, user3]
        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.pullAllUsers = \(Handlers.Base.MkOffset offset) (Handlers.Base.MkLimit limit) -> get >>= pure . Right . take limit . drop offset
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              mkGoodResponse = testBuilder
            } ::
            Handle (State [User])

    it "All clients can get a list of users" $ do
      --
      let baseHandle' = baseHandle
          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2)
          clientAdminUser3 = Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)
          clientAdminUser4 = Client Nothing Nothing Nothing

          webHandle1 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1
              }
          webHandle2 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser2
              }
          webHandle3 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser3
              }
          webHandle4 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser4
              }

      evalState (doLogic webHandle1 req') usersInBase
        `shouldBe` (testBuilder . userToWeb $ usersInBase)
      evalState (doLogic webHandle2 req') usersInBase
        `shouldBe` (testBuilder . userToWeb $ usersInBase)
      evalState (doLogic webHandle3 req') usersInBase
        `shouldBe` (testBuilder . userToWeb $ usersInBase)
      evalState (doLogic webHandle4 req') usersInBase
        `shouldBe` (testBuilder . userToWeb $ usersInBase)

    it "Client can panigate" $ do
      let req' = req {rawPathInfo = "/users", queryString = [("panigate", Just "{\"offset\":1,\"limit\":1}")]}
          baseHandle' = baseHandle
          client' = Client Nothing Nothing Nothing

          webHandle' =
            webHandle
              { base = baseHandle',
                client = client'
              }

      evalState (doLogic webHandle' req') usersInBase
        `shouldBe` (testBuilder . userToWeb $ take 1 $ drop 1 usersInBase)

  describe "EndPoint: /categories" $ do
    --- todo
    let req = defaultRequest
        req' = req {rawPathInfo = "/categories", queryString = [("panigate", Just "{\"offset\":0,\"limit\":10}")]}
        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

        categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]

        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.pullAllCategories = \(Handlers.Base.MkOffset offset) (Handlers.Base.MkLimit limit) -> get >>= pure . Right . take limit . drop offset
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              mkGoodResponse = testBuilder
            } ::
            Handle (State [Category])

    it "All clients can get a list of category" $ do
      let baseHandle' = baseHandle
          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2)
          clientAdminUser3 = Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)
          clientAdminUser4 = Client Nothing Nothing Nothing

          webHandle1 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1
              }
          webHandle2 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser2
              }
          webHandle3 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser3
              }
          webHandle4 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser4
              }

      evalState (doLogic webHandle1 req') categoriesInBase
        `shouldBe` (testBuilder . categoryToWeb $ categoriesInBase)
      evalState (doLogic webHandle2 req') categoriesInBase
        `shouldBe` (testBuilder . categoryToWeb $ categoriesInBase)
      evalState (doLogic webHandle3 req') categoriesInBase
        `shouldBe` (testBuilder . categoryToWeb $ categoriesInBase)
      evalState (doLogic webHandle4 req') categoriesInBase
        `shouldBe` (testBuilder . categoryToWeb $ categoriesInBase)

    it "Client can panigate" $ do
      let req' = req {rawPathInfo = "/categories", queryString = [("panigate", Just "{\"offset\":1,\"limit\":1}")]}
          baseHandle' = baseHandle
          client' = Client Nothing Nothing Nothing

          webHandle' =
            webHandle
              { base = baseHandle',
                client = client'
              }

      evalState (doLogic webHandle' req') categoriesInBase
        `shouldBe` (testBuilder . categoryToWeb $ take 1 $ drop 1 categoriesInBase)

  -- curl "127.0.0.1:4221/images?id=1" --output -

  describe "EndPoint: /images" $ do
    --- todo
    let req = defaultRequest
        req' = req {rawPathInfo = "/images", queryString = [("id", Just "1")]}
        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

        imagesInBase = [image1, image2, image3]

        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.pullImage = \(MkNumberImage num) -> get >>= pure . Right . Just . flip (!!) (fromIntegral num)
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              mkResponseForImage = testImage
            } ::
            Handle (State [Image])

    it "All clients can get a image" $ do
      let baseHandle' = baseHandle
          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2)
          clientAdminUser3 = Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)
          clientAdminUser4 = Client Nothing Nothing Nothing

          webHandle1 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1
              }
          webHandle2 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser2
              }
          webHandle3 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser3
              }
          webHandle4 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser4
              }

      evalState (doLogic webHandle1 req') imagesInBase
        `shouldBe` testImage (imagesInBase !! 1)
      evalState (doLogic webHandle2 req') imagesInBase
        `shouldBe` testImage (imagesInBase !! 1)
      evalState (doLogic webHandle3 req') imagesInBase
        `shouldBe` testImage (imagesInBase !! 1)
      evalState (doLogic webHandle4 req') imagesInBase
        `shouldBe` testImage (imagesInBase !! 1)

  describe "EndPoint: /users/create" $ do
    let req = defaultRequest
        req' = req {rawPathInfo = "/users/create"}
        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

        usersInBase = [user1, user2, user3]
        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.getTime = pure (read $(localtimeTemplate)),
              Handlers.Base.findUserByLogin = \(MkLogin login) ->
                gets
                  ( Right
                      . listToMaybe
                      . mapMaybe
                        ( \user@(User _ l _ _ _ _) ->
                            if l == login then Just user else Nothing
                        )
                  ),
              Handlers.Base.putUser = \(UserInternal name login pass admin publish) time -> pure $ Right Handlers.Base.Put
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              response404 = test404,
              response200 = test200
            } ::
            Handle (State [User])

    it "Admin can create new user" $ do
      let bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"Dager\",\"name\":\"Petr\",\"password\":\"qwerty\"}"
          baseHandle' = baseHandle
          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq
              }

      evalState (doLogic webHandle' req') usersInBase
        `shouldBe` test200

    it "Non-admin can't create a new user" $ do
      let bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"Dager\",\"name\":\"Petr\",\"password\":\"qwerty\"}"
          baseHandle' = baseHandle
          clientAdminUser1 = Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq
              }

      evalState (doLogic webHandle' req') usersInBase
        `shouldNotBe` test200

    it "Admin can't create a new user with login that already exists in the databse" $ do
      let clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          oldUser = E.encodeUtf8 . userLogin $ user1
          bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"" <> oldUser <> "\",\"name\":\"\",\"password\":\"qwerty\"}"
          baseHandle' = baseHandle
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq
              }

      evalState (doLogic webHandle' req') usersInBase
        `shouldNotBe` test200

  describe "EndPoint: /categories/create" $ do
    let req = defaultRequest
        req' = req {rawPathInfo = "/categories/create"}
        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

        categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]

        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.getTime = pure (read $(localtimeTemplate)),
              Handlers.Base.findCategoryByLabel = \(MkLabel label) -> do
                categories <- gets (map categoryLabel)
                pure $
                  Right $
                    if label `elem` categories
                      then Just undefined
                      else Nothing,
              Handlers.Base.putCategory = \(CategoryInternal label parent) -> pure $ Right Handlers.Base.Put
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              response404 = test404,
              response200 = test200
            } ::
            Handle (State [Category])

    it "Admin can create a new category" $ do
      let bodyReq = "{\"label\":\"Angel\",\"parent\":\"Abstract\"}"
          baseHandle' = baseHandle
          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq
              }

      evalState (doLogic webHandle' req') categoriesInBase
        `shouldBe` test200

    it "Non-admin can't create a new category" $ do
      let bodyReq = "{\"label\":\"Angel\",\"parent\":\"Abstract\"}"
          baseHandle' = baseHandle
          clientAdminUser1 = Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq
              }

      evalState (doLogic webHandle' req') categoriesInBase
        `shouldNotBe` test200

    it "Admin can't create a new category with label that already exists in the database" $ do
      let clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          oldLabel = E.encodeUtf8 . categoryLabel $ cat1
          bodyReq = "{\"label\":\"" <> oldLabel <> "\",\"parent\":\"Abstract\"}"
          baseHandle' = baseHandle
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq
              }

      evalState (doLogic webHandle' req') categoriesInBase
        `shouldNotBe` test200

    it "Admin can't create a new category with parent that does not exis in the database" $ do
      let clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          bodyReq = "{\"parent\":\"NOCATEGORYLABEL\",\"label\":\"NewLabel\"}"
          baseHandle' = baseHandle
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq
              }

      evalState (doLogic webHandle' req') categoriesInBase
        `shouldNotBe` test200

    it "Admin can create a new category without \"parent\" category" $ do
      let bodyReq1 = "{\"label\":\"Angel\",\"parent\":null}"
          bodyReq2 = "{\"label\":\"Angel\"}"
          baseHandle' = baseHandle
          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle1 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq1
              }
          webHandle2 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq2
              }

      evalState (doLogic webHandle1 req') categoriesInBase
        `shouldBe` test200
      evalState (doLogic webHandle2 req') categoriesInBase
        `shouldBe` test200

  describe "EndPoint: /categories/edit" $ do
    let req = defaultRequest
        req' = req {rawPathInfo = "/categories/edit"}
        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

        categoriesInBase = [cat1, cat2, cat3, cat4, cat5, cat6, cat7, cat8, cat9]

        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.getTime = pure (read $(localtimeTemplate)),
              Handlers.Base.findCategoryByLabel = \(MkLabel label) -> do
                categories <- gets (map categoryLabel)
                pure $
                  Right $
                    if label `elem` categories
                      then Just undefined
                      else Nothing,
              Handlers.Base.editCategory = \label (CategoryInternal newlabel parent) -> pure $ Right Handlers.Base.Change
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              response404 = test404,
              response200 = test200
            } ::
            Handle (State [Category])

    it "Admin can edit a category" $ do
      let bodyReq1 = "{\"label\":\"Man\",\"newLabel\":\"NewMan\",\"newparent\":\"Woman\"}"
          bodyReq2 = "{\"label\":\"Man\",\"newLabel\":\"NewMan\"}"
          bodyReq3 = "{\"label\":\"Man\",\"newparent\":\"Woman\"}"
          baseHandle' = baseHandle
          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle1 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq1
              }
          webHandle2 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq2
              }
          webHandle3 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1,
                getBody = const . pure $ bodyReq3
              }

      evalState (doLogic webHandle1 req') categoriesInBase
        `shouldBe` test200
      evalState (doLogic webHandle2 req') categoriesInBase
        `shouldBe` test200
      evalState (doLogic webHandle3 req') categoriesInBase
        `shouldBe` test200

    it "Non-admin can't edit a category" $ do
      let bodyReq = "{\"label\":\"Man\",\"newLabel\":\"NewMan\",\"newparent\":\"Woman\"}"
          baseHandle' = baseHandle
          clientAdminUser3 = Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser3,
                getBody = const . pure $ bodyReq
              }
      evalState (doLogic webHandle' req') categoriesInBase
        `shouldNotBe` test200

  describe "EndPoint: /news" $ do
    let req = defaultRequest
        req' = req {rawPathInfo = "/news", queryString = [("panigate", Just "{\"offset\":0,\"limit\":10}")]}
        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }
        newsInBase =
          [ MkNewsOut
              { nTitle = MkTitle "news1",
                nTime = read $(localtimeTemplate),
                nAuthor = MkName "user1",
                nCategories = map MkLabel ["Abstract", "Man"],
                nContent = MkContent "content1",
                nImages = [MkURI_Image "/images?id=1"],
                nIsPublish = True
              },
            MkNewsOut
              { nTitle = MkTitle "news2",
                nTime = read $(localtimeTemplate),
                nAuthor = MkName "user2",
                nCategories = map MkLabel ["Abstract", "Woman"],
                nContent = MkContent "content2",
                nImages = [],
                nIsPublish = True
              },
            MkNewsOut
              { nTitle = MkTitle "news3",
                nTime = read $(localtimeTemplate),
                nAuthor = MkName "user3",
                nCategories = map MkLabel ["Abstract", "Woman", "Witch"],
                nContent = MkContent "content3",
                nImages = [],
                nIsPublish = False
              },
            MkNewsOut
              { nTitle = MkTitle "news4",
                nTime = read $(localtimeTemplate),
                nAuthor = MkName "user1",
                nCategories = map MkLabel ["Abstract", "Woman", "Queen"],
                nContent = MkContent "content4",
                nImages = map MkURI_Image ["/images?id=2", "/images?id=3"],
                nIsPublish = True
              }
          ]

        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.sortColumnNews = DataNews,
              Handlers.Base.sortOrderNews = Descending,
              Handlers.Base.findSubString = Nothing,
              Handlers.Base.filtersNews = [],
              Handlers.Base.pullAllNews = \(Handlers.Base.MkOffset offset) (Handlers.Base.MkLimit limit) columnType sortOrder find filters -> get >>= pure . Right . take limit . drop offset
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              mkGoodResponse = testBuilder
            } ::
            Handle (State [NewsOut])

    it "All clients can get list of news" $ do
      --
      let baseHandle' = baseHandle
          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2)
          clientAdminUser3 = Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)
          clientAdminUser4 = Client Nothing Nothing Nothing

          webHandle1 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1
              }
          webHandle2 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser2
              }
          webHandle3 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser3
              }
          webHandle4 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser4
              }

      evalState (doLogic webHandle1 req') newsInBase
        `shouldBe` (testBuilder . newsToWeb $ newsInBase)
      evalState (doLogic webHandle2 req') newsInBase
        `shouldBe` (testBuilder . newsToWeb $ newsInBase)
      evalState (doLogic webHandle3 req') newsInBase
        `shouldBe` (testBuilder . newsToWeb $ newsInBase)
      evalState (doLogic webHandle4 req') newsInBase
        `shouldBe` (testBuilder . newsToWeb $ newsInBase)

    it "Client can panigate" $ do
      let req' = req {rawPathInfo = "/news", queryString = [("panigate", Just "{\"offset\":1,\"limit\":1}")]}
          baseHandle' = baseHandle
          client' = Client Nothing Nothing Nothing

          webHandle' =
            webHandle
              { base = baseHandle',
                client = client'
              }

      evalState (doLogic webHandle' req') newsInBase
        `shouldBe` (testBuilder . newsToWeb $ take 1 $ drop 1 newsInBase)

    it "Check default news settings" $ do
      let baseHandle' =
            baseHandle
              { Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters ->
                  pure . Right $
                    [ MkNewsOut
                        (MkTitle $ typeToText columnType)
                        testTime
                        (MkName $ typeToText sortOrder)
                        []
                        (MkContent $ typeToText find)
                        (map (MkURI_Image . typeToText) filters)
                        False
                    ]
              }

          client1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          client2 = Client Nothing Nothing Nothing
          webHandle1 =
            webHandle
              { base = baseHandle',
                client = client1
              }

          webHandle2 =
            webHandle
              { base = baseHandle',
                client = client2
              }

      evalState (doLogic webHandle1 req') newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText DataNews)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )

      evalState (doLogic webHandle2 req') newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText DataNews)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor $ Nothing]
                           False
                       ]
                   )

    it "Client can choouse the type of sorting" $ do
      let req1 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"DataNews\",\"sortOrder\":\"Ascending\"}")]}
          req2 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"DataNews\",\"sortOrder\":\"Descending\"}")]}
          req3 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"AuthorNews\",\"sortOrder\":\"Ascending\"}")]}
          req4 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"AuthorNews\",\"sortOrder\":\"Descending\"}")]}
          req5 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"CategoryName\",\"sortOrder\":\"Ascending\"}")]}
          req6 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"CategoryName\",\"sortOrder\":\"Descending\"}")]}
          req7 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"QuantityImages\",\"sortOrder\":\"Ascending\"}")]}
          req8 = req {rawPathInfo = "/news", queryString = [("sort", Just "{\"columnType\":\"QuantityImages\",\"sortOrder\":\"Descending\"}")]}

          baseHandle' =
            baseHandle
              { Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters ->
                  pure . Right $
                    [ MkNewsOut
                        (MkTitle $ typeToText columnType)
                        testTime
                        (MkName $ typeToText sortOrder)
                        []
                        (MkContent $ typeToText find)
                        (map (MkURI_Image . typeToText) filters)
                        False
                    ]
              }

          client1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = client1
              }

      evalState (doLogic webHandle' req1) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText DataNews)
                           testTime
                           (MkName $ typeToText Ascending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )

      evalState (doLogic webHandle' req2) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText DataNews)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )
      evalState (doLogic webHandle' req3) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText AuthorNews)
                           testTime
                           (MkName $ typeToText Ascending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )
      evalState (doLogic webHandle' req4) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       -- [(typeToText AuthorNews, testTime, typeToText Descending, [], typeToText (Nothing :: Maybe Find), [typeToText . FilterPublishOrAuthor . Just . userLogin $ user1], False)])

                       [ MkNewsOut
                           (MkTitle $ typeToText AuthorNews)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )
      evalState (doLogic webHandle' req5) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       -- [(typeToText CategoryName, testTime, typeToText Ascending, [], typeToText (Nothing :: Maybe Find), [typeToText . FilterPublishOrAuthor . Just . userLogin $ user1], False)])

                       [ MkNewsOut
                           (MkTitle $ typeToText CategoryName)
                           testTime
                           (MkName $ typeToText Ascending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )
      evalState (doLogic webHandle' req6) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText CategoryName)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )
      evalState (doLogic webHandle' req7) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText QuantityImages)
                           testTime
                           (MkName $ typeToText Ascending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )
      evalState (doLogic webHandle' req8) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText QuantityImages)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )

    it "Client can search by string" $ do
      let req' = req {rawPathInfo = "/news", queryString = [("find", Just "{\"subString\":\"googleIt\"}")]}

          baseHandle' =
            baseHandle
              { Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters ->
                  pure . Right $
                    [ MkNewsOut
                        (MkTitle $ typeToText columnType)
                        testTime
                        (MkName $ typeToText sortOrder)
                        []
                        (MkContent $ typeToText find)
                        (map (MkURI_Image . typeToText) filters)
                        False
                    ]
              }

          client1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = client1
              }

      evalState (doLogic webHandle' req') newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText DataNews)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Just $ Find "googleIt"))
                           [MkURI_Image . typeToText . FilterPublishOrAuthor . Just . userLogin $ user1]
                           False
                       ]
                   )

    it "Client can filter news by date" $ do
      let req1 =
            req
              { rawPathInfo = "/news",
                queryString =
                  [ ( "filter",
                      Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataAt\"}]"
                    )
                  ]
              }
          req2 =
            req
              { rawPathInfo = "/news",
                queryString =
                  [ ( "filter",
                      Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataUntil\"}]"
                    )
                  ]
              }
          req3 =
            req
              { rawPathInfo = "/news",
                queryString =
                  [ ( "filter",
                      Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataSince\"}]"
                    )
                  ]
              }

          baseHandle' =
            baseHandle
              { Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters ->
                  pure . Right $
                    [ MkNewsOut
                        (MkTitle $ typeToText columnType)
                        testTime
                        (MkName $ typeToText sortOrder)
                        []
                        (MkContent $ typeToText find)
                        (map (MkURI_Image . typeToText) filters)
                        False
                    ]
              }

          client1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = client1
              }

      evalState (doLogic webHandle' req1) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText DataNews)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           ( map MkURI_Image $
                               (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1)
                                 : [typeToText . FilterDataAt $ testDay]
                           )
                           False
                       ]
                   )

      evalState (doLogic webHandle' req2) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText DataNews)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           ( map MkURI_Image $
                               (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1)
                                 : [typeToText . FilterDataUntil $ testDay]
                           )
                           False
                       ]
                   )

      evalState (doLogic webHandle' req3) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText DataNews)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           ( map MkURI_Image $
                               (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1)
                                 : [typeToText . FilterDataSince $ testDay]
                           )
                           False
                       ]
                   )

    it "Client can filter news by data, by author name, by category label, by string in title, by string in content" $ do
      -- todo
      let req1 =
            req
              { rawPathInfo = "/news",
                queryString =
                  [ ( "filter",
                      Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataAt\"},{\"contents\":\"Vasya\",\"tag\":\"FilterAuthorName\"},{\"contents\":\"Man\",\"tag\":\"FilterCategoryLabel\"},{\"contents\":\"a\",\"tag\":\"FilterTitleFind\"},{\"contents\":\"b\",\"tag\":\"FilterContentFind\"}]"
                    )
                  ]
              }

          baseHandle' =
            baseHandle
              { Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters ->
                  pure . Right $
                    [ MkNewsOut
                        (MkTitle $ typeToText columnType)
                        testTime
                        (MkName $ typeToText sortOrder)
                        []
                        (MkContent $ typeToText find)
                        (map (MkURI_Image . typeToText) filters)
                        False
                    ]
              }

          client1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = client1
              }

      evalState (doLogic webHandle' req1) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText DataNews)
                           testTime
                           (MkName $ typeToText Descending)
                           []
                           (MkContent $ typeToText (Nothing :: Maybe Find))
                           ( map MkURI_Image $
                               (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1)
                                 : (typeToText . FilterDataAt $ testDay)
                                 : (typeToText . FilterAuthorName $ "Vasya")
                                 : (typeToText . FilterCategoryLabel $ "Man")
                                 : (typeToText . FilterTitleFind $ "a")
                                 : (typeToText . FilterContentFind $ "b")
                                 : []
                           )
                           False
                       ]
                   )

    it "Client can filter, sort and search in one request" $ do
      -- todo
      let req1 =
            req
              { rawPathInfo = "/news",
                queryString =
                  [ ("sort", Just "{\"columnType\":\"CategoryName\",\"sortOrder\":\"Ascending\"}"),
                    ("find", Just "{\"subString\":\"googleIt\"}"),
                    ( "filter",
                      Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataAt\"},{\"contents\":\"Vasya\",\"tag\":\"FilterAuthorName\"},{\"contents\":\"Man\",\"tag\":\"FilterCategoryLabel\"},{\"contents\":\"a\",\"tag\":\"FilterTitleFind\"},{\"contents\":\"b\",\"tag\":\"FilterContentFind\"}]"
                    )
                  ]
              }

          baseHandle' =
            baseHandle
              { Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters ->
                  pure . Right $
                    [ MkNewsOut
                        (MkTitle $ typeToText columnType)
                        testTime
                        (MkName $ typeToText sortOrder)
                        []
                        (MkContent $ typeToText find)
                        (map (MkURI_Image . typeToText) filters)
                        False
                    ]
              }

          client1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = client1
              }

      -- data NewsOut = MkNewsOut Title UTCTime Name [Label] Content [URI_Image] Bool
      -- newsToWeb :: [NewsOut] -> Builder
      -- newsToWeb = fromLazyByteString . encode @[NewsToWeb] . map convertToWeb
      --   where
      --     convertToWeb :: NewsOut -> NewsToWeb
      --     convertToWeb (MkNewsOut (MkTitle t) d (MkName n) ls (MkContent c) im b) = NewsToWeb t d n (map getLabel ls) c (map getURI_Image im) b
      -- --
      evalState (doLogic webHandle' req1) newsInBase
        `shouldBe` ( testBuilder . newsToWeb $
                       [ MkNewsOut
                           (MkTitle $ typeToText CategoryName)
                           testTime
                           (MkName $ typeToText Ascending)
                           []
                           (MkContent $ typeToText (Just $ Find "googleIt"))
                           ( map MkURI_Image $
                               (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1)
                                 : (typeToText . FilterDataAt $ testDay)
                                 : (typeToText . FilterAuthorName $ "Vasya")
                                 : (typeToText . FilterCategoryLabel $ "Man")
                                 : (typeToText . FilterTitleFind $ "a")
                                 : (typeToText . FilterContentFind $ "b")
                                 : []
                           )
                           False
                       ]
                   )

  describe "EndPoint: /news/create" $ do
    let req = defaultRequest
        req' = req {rawPathInfo = "/news/create"}

        bodyReq = "{\"title\":\"News from SH script\",\"isPublish\":false,\"login\":\"login1\",\"label\":\"Witch\",\"content\":\"New text about news from sh\",\"images\":[{\"imageHeader\":\"image\",\"imageBase64\":\"kartinka for news sh\"},{\"imageHeader\":\"image2 sh\",\"imageBase64\":\"kartinka for news sh\"}]}"

        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.getTime = pure (read $(localtimeTemplate)),
              Handlers.Base.findNewsByTitle = const (pure $ Right Nothing),
              Handlers.Base.findUserByLogin = const (pure $ Right $ Just user1),
              Handlers.Base.findCategoryByLabel = const (pure . Right $ Just cat1),
              Handlers.Base.putNews = \(NewsInternal title login label content images isPublish) time -> pure $ Right Handlers.Base.Put
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              response404 = test404,
              response200 = test200,
              getBody = const . pure $ bodyReq
            } ::
            Handle Identity

    it "Publisher can create news" $ do
      let baseHandle' = baseHandle
          clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser2
              }
      runIdentity (doLogic webHandle' req')
        `shouldBe` test200

    it "Non-publisher can't create news" $ do
      let baseHandle' = baseHandle
          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          webHandle' =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1
              }

      runIdentity (doLogic webHandle' req')
        `shouldNotBe` test200

  describe "EndPoint: /news/edit" $ do
    let req = defaultRequest
        req' = req {rawPathInfo = "/news/edit"}

        bodyReq = "{\"title\":\"News 1 about Witch from user 1\",\"newTitle\":\"Edit News1\",\"newIsPublish\":true,\"newLogin\":\"login3\",\"newLabel\":\"Man\",\"newContent\":\"New Content\",\"images\":[{\"imageHeader\":\"edit image\",\"imageBase64\":\"edit kartinka for news sh\"}]}"

        logHandle =
          Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

        newsInBase = [news1, news2, news3, news4]

        baseHandle =
          Handlers.Base.Handle
            { Handlers.Base.logger = logHandle,
              Handlers.Base.findNewsByTitle = \title -> do
                titles <- gets (map (MkTitle . newsTitle))
                pure $
                  Right $
                    if title `elem` titles
                      then Just undefined
                      else Nothing,
              Handlers.Base.findUserByLogin = const (pure $ Right $ Just user1),
              Handlers.Base.findCategoryByLabel = const (pure $ Right $ Just cat1),
              Handlers.Base.editNews = \title time (NewsEditInternal mbtitle mblogin mblabel mbcontent image mbp) -> pure $ Right Handlers.Base.Change
            }
        webHandle =
          Handle
            { logger = logHandle,
              base = baseHandle,
              response200 = test200,
              response404 = test404,
              getBody = const . pure $ bodyReq
            } ::
            Handle (State [News])

    it "Author can edit news" $ do
      let baseHandle' = baseHandle {Handlers.Base.validCopyRight = \login title -> pure $ Right True}

          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2)
          clientAdminUser3 = Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)

          webHandle1 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1
              }
          webHandle2 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser2
              }
          webHandle3 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser3
              }

      evalState (doLogic webHandle1 req') newsInBase
        `shouldBe` test200
      evalState (doLogic webHandle2 req') newsInBase
        `shouldBe` test200
      evalState (doLogic webHandle3 req') newsInBase
        `shouldBe` test200

    it "Non-author can't edit news" $ do
      let baseHandle' = baseHandle {Handlers.Base.validCopyRight = \login title -> pure $ Right False}

          clientAdminUser1 = Client (Just Proxy) Nothing (Just . MkLogin $ userLogin user1)
          clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just . MkLogin $ userLogin user2)
          clientAdminUser3 = Client Nothing (Just Proxy) (Just . MkLogin $ userLogin user3)
          clientAdminUser4 = Client Nothing Nothing Nothing

          webHandle1 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser1
              }
          webHandle2 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser2
              }
          webHandle3 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser3
              }
          webHandle4 =
            webHandle
              { base = baseHandle',
                client = clientAdminUser4
              }

      evalState (doLogic webHandle1 req') newsInBase
        `shouldNotBe` test200
      evalState (doLogic webHandle2 req') newsInBase
        `shouldNotBe` test200
      evalState (doLogic webHandle3 req') newsInBase
        `shouldNotBe` test200
      evalState (doLogic webHandle4 req') newsInBase
        `shouldNotBe` test200

testTime :: UTCTime
testTime = read $(localtimeTemplate)

testDay :: Day
testDay = fromGregorian 2023 1 1

typeToText :: (Show a) => a -> T.Text
typeToText = T.pack . show

test404 :: Response
test404 = responseBuilder notFound404 [] "Not ok. status 404\n"

test200 :: Response
test200 = responseBuilder status200 [] "All ok. status 200\n"

testBuilder :: Builder -> Response
testBuilder = responseBuilder status200 []

testImage :: Image -> Response
testImage (Image header base) = responseBuilder status200 [(hContentType, contentType)] content
  where
    contentType = E.encodeUtf8 header
    content = BU.fromByteString . B64.decodeBase64Lenient . E.encodeUtf8 $ base

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s, show h, show b]
  show _ = undefined

instance Eq Response where
  (==) (ResponseBuilder s h b) (ResponseBuilder s' h' b') = (s == s') && (h == h') && (show b == show b')
  (==) _ _ = undefined

-- data Response
--     = ResponseFile H.Status H.ResponseHeaders FilePath (Maybe FilePart)
--     | ResponseBuilder H.Status H.ResponseHeaders Builder
--     | ResponseStream H.Status H.ResponseHeaders StreamingBody
--     | ResponseRaw (IO B.ByteString -> (B.ByteString -> IO ()) -> IO ()) Response
--   deriving Typeable
