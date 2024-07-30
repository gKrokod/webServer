{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
module Handlers.WebLogicSpec (spec) where

import Scheme
import Base.FillTables (user1, user2, user3, cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat9,cat8, news1,news2,news3,news4, image1,image2,image3)
import Test.Hspec
import Handlers.WebLogic
import Scheme (ColumnType(..), SortOrder(..))
import Web.WebType (userToWeb, categoryToWeb, newsToWeb)
import Base.LocalTime (localtimeTemplate)
import qualified Handlers.Logger 
import qualified Handlers.Base
-- import qualified Logger 
import Control.Monad.State
import Test.QuickCheck
import Data.Maybe
import Data.Either (isLeft)
import Network.Wai (defaultRequest, Request, rawPathInfo, queryString, requestHeaders, rawQueryString, queryString)
import Network.Wai (getRequestBodyChunk, responseBuilder)
import Network.Wai.Internal (Response(..))
import Network.HTTP.Types (notFound404, status200, hContentType)
import Data.Proxy
import qualified Data.Text.Encoding as E
import qualified Data.Text as T

import Data.Binary.Builder as BU (Builder, fromByteString)
import Data.ByteString.Base64 as B64
import Data.Time (UTCTime(..), Day(..), fromGregorian )
--
--
--

typeToText :: (Show a) => a -> T.Text
typeToText = T.pack . show

test404 :: Response
test404 = responseBuilder notFound404 [] "Not ok. status 404\n" 

test200 :: Response
test200 = responseBuilder status200 [] "All ok. status 200\n" 

testBuilder = responseBuilder status200 []

testImage :: Image -> Response
testImage (Image header base) = responseBuilder status200 [(hContentType, contentType)] content
  where 
    contentType = E.encodeUtf8 header
    content = BU.fromByteString . B64.decodeBase64Lenient . E.encodeUtf8 $ base 

instance Show Response where
  show (ResponseBuilder s h b) = mconcat [show s,show h, show b]
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

spec :: Spec
spec = do
  describe "Autorization:" $ do
    -- user1 admin noPublisher
    -- user2 admin publisher
    -- user3 noAdmin publisher
    -- unknown user noAdmin noPublisher
    --
-- ("Authorization","Basic bG9naW4xOnFwYXNzMQ=="),
-- requestHeaders = [("Host","127.0.0.1:4221"),("Authorization","Basic bG9naW4xOnFwYXNzMQ=="),("User-Agent","curl/7.68.0"),("Accept","*/*")]-- user1
-- Authorization","Basic bG9naW4yOnFwYXNzMg==")-- user2
-- "Authorization","Basic bG9naW4zOnFwYXNzMw==")-- user3
--
      let req = defaultRequest

      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

      let usersInBase = [user1, user2, user3] 
      let baseHandle  = Handlers.Base.Handle
            {
               Handlers.Base.logger = logHandle,
               Handlers.Base.findUserByLogin = \login -> do
                 users <- get
                 pure 
                   (Right 
                     $ listToMaybe 
                      $ mapMaybe (\user@(User _ l _ _ _ _) -> if l == login
                                                            then Just user else Nothing) $ users),
               Handlers.Base.validPassword = \login password -> pure $ Right True
                                                      } 
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               response404 = test404 
                                                      }  :: Handle (State [User])

      it "Unknow users do not received privileges" $ do
          let req' = req {requestHeaders = []} 
          let webHandle' = webHandle 

          client <$> (evalState (doAutorization webHandle' req') usersInBase)
              `shouldBe` 
                  Right (Client Nothing Nothing Nothing)

      it "Users with the correct password receive their privileges correctly" $ do
          let req1 = req {requestHeaders = [("Authorization","Basic bG9naW4xOnFwYXNzMQ==")]} -- user1
          let req2 = req {requestHeaders = [("Authorization","Basic bG9naW4yOnFwYXNzMg==")]} -- user2
          let req3 = req {requestHeaders = [("Authorization","Basic bG9naW4zOnFwYXNzMw==")]} -- user3

          let webHandle' = webHandle 

          client <$> (evalState (doAutorization webHandle' req1) usersInBase)
              `shouldBe` 
                  Right (Client (Just Proxy) Nothing (Just $ userLogin user1)) -- user1

          client <$> (evalState (doAutorization webHandle' req2) usersInBase)
              `shouldBe` 
                  Right (Client (Just Proxy) (Just Proxy) (Just $ userLogin user2)) -- user2

          client <$> (evalState (doAutorization webHandle' req3) usersInBase)
              `shouldBe` 
                  Right (Client Nothing (Just Proxy)  (Just $ userLogin user3)) -- user3

      it "Users with the uncorrect password do not receive their privileges" $ do
          let req1 = req {requestHeaders = [("Authorization","Basic bG9naW4xOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user1
          let req2 = req {requestHeaders = [("Authorization","Basic bG9naW4yOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user2
          let req3 = req {requestHeaders = [("Authorization","Basic bG9naW4zOk5PQ09SUkVDVFBBU1NXT1JE")]} -- user3
          let baseHandle' = baseHandle {Handlers.Base.validPassword = \login password -> pure $ Right False}
          let webHandle' = webHandle {base = baseHandle'}
               
          client <$> (evalState (doAutorization webHandle' req1) usersInBase)
              `shouldNotBe` 
                  Right (Client (Just Proxy) Nothing (Just $ userLogin user1)) -- user1

          client <$> (evalState (doAutorization webHandle' req2) usersInBase)
              `shouldNotBe` 
                  Right (Client (Just Proxy) (Just Proxy) (Just $ userLogin user2)) -- user2

          client <$> (evalState (doAutorization webHandle' req3) usersInBase)
              `shouldNotBe` 
                  Right (Client Nothing (Just Proxy)  (Just $ userLogin user3)) -- user3

-- doAutorization :: (Monad m) => Handle m -> Request -> m (Either Response (Handle m))
  describe "EndPoint: /users" $ do
      let req = defaultRequest
      let req' = req {rawPathInfo = "/users", queryString= [("panigate", Just "{\"offset\":0,\"limit\":7}")]}
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

      let usersInBase = [user1, user2, user3] 
      let baseHandle  = Handlers.Base.Handle
            {
               Handlers.Base.logger = logHandle,
               Handlers.Base.pullAllUsers = \offset limit -> get >>= pure . Right . take limit . drop offset
                                                      } 
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               mkGoodResponse = testBuilder
                                                      }  :: Handle (State [User])

      it "All client may get list of users" $ do
--
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just $ userLogin user2)
          let clientAdminUser3 = Client Nothing (Just Proxy) (Just $ userLogin user3)
          let clientAdminUser4 = Client Nothing Nothing Nothing

          let webHandle1 = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 }
          let webHandle2 = webHandle {base = baseHandle'
                                     , client = clientAdminUser2 }
          let webHandle3 = webHandle {base = baseHandle'
                                     , client = clientAdminUser3 }
          let webHandle4 = webHandle {base = baseHandle'
                                     , client = clientAdminUser4 }

          (evalState (doLogic webHandle1 req') usersInBase)
              `shouldBe` 
                  (testBuilder . userToWeb $ usersInBase)
          (evalState (doLogic webHandle2 req') usersInBase)
              `shouldBe` 
                  (testBuilder . userToWeb $ usersInBase)
          (evalState (doLogic webHandle3 req') usersInBase)
              `shouldBe` 
                  (testBuilder . userToWeb $ usersInBase)
          (evalState (doLogic webHandle4 req') usersInBase)
              `shouldBe` 
                  (testBuilder . userToWeb $ usersInBase)

      it "Client can panigate" $ do
          
          let req' = req {rawPathInfo = "/users", queryString= [("panigate", Just "{\"offset\":1,\"limit\":1}")]}
          let baseHandle' = baseHandle
          let client' = Client Nothing Nothing Nothing 

          let webHandle' = webHandle {base = baseHandle'
                                     , client = client' }

          (evalState (doLogic webHandle' req') usersInBase)
              `shouldBe` 
                  (testBuilder . userToWeb $ take 1 $ drop 1 usersInBase)

  describe "EndPoint: /categories" $ do --- todo
      let req = defaultRequest
      let req' = req {rawPathInfo = "/categories", queryString= [("panigate", Just "{\"offset\":0,\"limit\":10}")]}
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

      let categoriesInBase = [cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9]

      let baseHandle  = Handlers.Base.Handle
            {
               Handlers.Base.logger = logHandle,
               Handlers.Base.pullAllCategories = \offset limit -> get >>= pure . Right . take limit . drop offset
                                                      } 
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               mkGoodResponse = testBuilder
                                                      }  :: Handle (State [Category])

      it "All client may get list of category" $ do
--
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just $ userLogin user2)
          let clientAdminUser3 = Client Nothing (Just Proxy) (Just $ userLogin user3)
          let clientAdminUser4 = Client Nothing Nothing Nothing

          let webHandle1 = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 }
          let webHandle2 = webHandle {base = baseHandle'
                                     , client = clientAdminUser2 }
          let webHandle3 = webHandle {base = baseHandle'
                                     , client = clientAdminUser3 }
          let webHandle4 = webHandle {base = baseHandle'
                                     , client = clientAdminUser4 }

          (evalState (doLogic webHandle1 req') categoriesInBase)
              `shouldBe` 
                  (testBuilder . categoryToWeb $ categoriesInBase)
          (evalState (doLogic webHandle2 req') categoriesInBase)
              `shouldBe` 
                  (testBuilder . categoryToWeb $ categoriesInBase)
          (evalState (doLogic webHandle3 req') categoriesInBase)
              `shouldBe` 
                  (testBuilder . categoryToWeb $ categoriesInBase)
          (evalState (doLogic webHandle4 req') categoriesInBase)
              `shouldBe` 
                  (testBuilder . categoryToWeb $ categoriesInBase)

      it "Client can panigate" $ do
          
          let req' = req {rawPathInfo = "/categories", queryString= [("panigate", Just "{\"offset\":1,\"limit\":1}")]}
          let baseHandle' = baseHandle
          let client' = Client Nothing Nothing Nothing 

          let webHandle' = webHandle {base = baseHandle'
                                     , client = client' }

          (evalState (doLogic webHandle' req') categoriesInBase)
              `shouldBe` 
                  (testBuilder . categoryToWeb $ take 1 $ drop 1 categoriesInBase)

-- curl "127.0.0.1:4221/images?id=1" --output -

  describe "EndPoint: /images" $ do --- todo
      let req = defaultRequest
      let req' = req {rawPathInfo = "/images", queryString= [("id", Just "1")]}
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

      let imagesInBase = [image1,image2,image3]

      let baseHandle  = Handlers.Base.Handle
            {
               Handlers.Base.logger = logHandle,
               Handlers.Base.pullImage = \num -> get >>= pure . Right . Just . flip (!!) (fromIntegral num)
                                                      } 
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               mkResponseForImage = testImage
                                                      }  :: Handle (State [Image])

      it "All client may get image" $ do
--
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just $ userLogin user2)
          let clientAdminUser3 = Client Nothing (Just Proxy) (Just $ userLogin user3)
          let clientAdminUser4 = Client Nothing Nothing Nothing

          let webHandle1 = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 }
          let webHandle2 = webHandle {base = baseHandle'
                                     , client = clientAdminUser2 }
          let webHandle3 = webHandle {base = baseHandle'
                                     , client = clientAdminUser3 }
          let webHandle4 = webHandle {base = baseHandle'
                                     , client = clientAdminUser4 }

          (evalState (doLogic webHandle1 req') imagesInBase)
              `shouldBe` 
                  (testImage $ (imagesInBase !! 1))
          (evalState (doLogic webHandle2 req') imagesInBase)
              `shouldBe` 
                  (testImage $ (imagesInBase !! 1))
          (evalState (doLogic webHandle3 req') imagesInBase)
              `shouldBe` 
                  (testImage $ (imagesInBase !! 1))
          (evalState (doLogic webHandle4 req') imagesInBase)
              `shouldBe` 
                  (testImage $ (imagesInBase !! 1))

  describe "EndPoint: /users/create" $ do
      let req = defaultRequest
      let req' = req {rawPathInfo = "/users/create"}
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

      let usersInBase = [user1, user2, user3] 
      let baseHandle  = Handlers.Base.Handle
            {
               Handlers.Base.logger = logHandle,
               Handlers.Base.getTime = pure (read $(localtimeTemplate)), 
               Handlers.Base.findUserByLogin = \login -> do
                 users <- get
                 pure 
                   (Right 
                     $ listToMaybe 
                      $ mapMaybe (\user@(User _ l _ _ _ _) -> if l == login
                                                            then Just user else Nothing) $ users),
               Handlers.Base.putUser = \name login pass time admin publish -> pure $ Right Handlers.Base.Put
                                                      } 
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               response404 = test404, 
               response200 = test200 
                                                      }  :: Handle (State [User])

      it "admin can create new user" $ do
          let bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"Dager\",\"name\":\"Petr\",\"password\":\"qwerty\"}"
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq}

          (evalState (doLogic webHandle' req') usersInBase)
              `shouldBe` 
                  (test200)

      it "No admin can't create new user" $ do
          let bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"Dager\",\"name\":\"Petr\",\"password\":\"qwerty\"}"
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client Nothing (Just Proxy) (Just $ userLogin user3)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq}

          (evalState (doLogic webHandle' req') usersInBase)
              `shouldNotBe` 
                  (test200)

      it "admin can't create new user with login already exist" $ do
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let oldUser = E.encodeUtf8 . userLogin $ user1
          let bodyReq = "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"" <> oldUser <> "\",\"name\":\"\",\"password\":\"qwerty\"}"
          let baseHandle' = baseHandle
          let webHandle' = webHandle {base = baseHandle'
            , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq}

          (evalState (doLogic webHandle' req') usersInBase)
              `shouldNotBe` 
                  (test200)

  describe "EndPoint: /categories/create" $ do
      let req = defaultRequest
      let req' = req {rawPathInfo = "/categories/create"}
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

      let categoriesInBase = [cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9]

      let baseHandle  = Handlers.Base.Handle
            {
               Handlers.Base.logger = logHandle,
               Handlers.Base.getTime = pure (read $(localtimeTemplate)), 
               Handlers.Base.findCategoryByLabel = \label  -> do
                 categories <- map categoryLabel <$> get
                 pure $ Right $ 
                   if label `elem` categories then Just (Category label undefined)
                                              else Nothing,
               Handlers.Base.putCategory = \label parent -> pure $ Right Handlers.Base.Put
                                                      } 
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               response404 = test404, 
               response200 = test200 
                                                      }  :: Handle (State [Category])

-- curl -v -X POST 127.0.0.1:4221/categories/create -H "Content-Type: application/json" -d '{"label":"Angel","parent":"Abstract"}'
-- curl -v -X POST 127.0.0.1:4221/categories/create -H "Content-Type: application/json" -d '{"label":"NewAbstract","parent":null}'
--
      it "admin can create new category" $ do
          let bodyReq = "{\"label\":\"Angel\",\"parent\":\"Abstract\"}"
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq}

          (evalState (doLogic webHandle' req') categoriesInBase)
              `shouldBe` 
                  (test200)

      it "No admin can't create new category" $ do
          let bodyReq = "{\"label\":\"Angel\",\"parent\":\"Abstract\"}"
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client Nothing (Just Proxy) (Just $ userLogin user3)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq}

          (evalState (doLogic webHandle' req') categoriesInBase)
              `shouldNotBe` 
                  (test200)

      it "admin can't create new category with label already exist" $ do
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let oldLabel = E.encodeUtf8 . categoryLabel $ cat1
          let bodyReq = "{\"label\":\"" <> oldLabel <> "\",\"parent\":\"Abstract\"}"
          let baseHandle' = baseHandle
          let webHandle' = webHandle {base = baseHandle'
            , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq}

          (evalState (doLogic webHandle' req') categoriesInBase)
              `shouldNotBe` 
                  (test200)

      it "admin can't create new category with parent don't exist" $ do
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let bodyReq = "{\"parent\":\"NOCATEGORYLABEL\",\"label\":\"NewLabel\"}"
          let baseHandle' = baseHandle
          let webHandle' = webHandle {base = baseHandle'
            , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq}

          (evalState (doLogic webHandle' req') categoriesInBase)
              `shouldNotBe` 
                  (test200)

      it "admin can create new category without parent" $ do
          let bodyReq1 = "{\"label\":\"Angel\",\"parent\":null}"
          let bodyReq2 = "{\"label\":\"Angel\"}"
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let webHandle1 = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq1}
          let webHandle2 = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq2}

          (evalState (doLogic webHandle1 req') categoriesInBase)
              `shouldBe` 
                  (test200)
          (evalState (doLogic webHandle2 req') categoriesInBase)
              `shouldBe` 
                  (test200)

  describe "EndPoint: /categories/edit" $ do
      let req = defaultRequest
      let req' = req {rawPathInfo = "/categories/edit"}
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

      let categoriesInBase = [cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat8,cat9]

      let baseHandle  = Handlers.Base.Handle
            {
               Handlers.Base.logger = logHandle,
               Handlers.Base.getTime = pure (read $(localtimeTemplate)), 
               Handlers.Base.findCategoryByLabel = \label  -> do
                 categories <- map categoryLabel <$> get
                 pure $ Right $ 
                   if label `elem` categories then Just (Category label undefined)
                                              else Nothing,
               Handlers.Base.editCategory = \label newlabel parent -> pure $ Right Handlers.Base.Change
                                                      } 
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               response404 = test404, 
               response200 = test200 
                                                      }  :: Handle (State [Category])

      it "admin can edit category" $ do
          let bodyReq1 = "{\"label\":\"Man\",\"newLabel\":\"NewMan\",\"newparent\":\"Woman\"}"
          let bodyReq2 = "{\"label\":\"Man\",\"newLabel\":\"NewMan\"}"
          let bodyReq3 = "{\"label\":\"Man\",\"newparent\":\"Woman\"}"
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let webHandle1 = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq1}
          let webHandle2 = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq2}
          let webHandle3 = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 
                                     , getBody = const . pure $ bodyReq3}

          (evalState (doLogic webHandle1 req') categoriesInBase)
              `shouldBe` 
                  (test200)
          (evalState (doLogic webHandle2 req') categoriesInBase)
              `shouldBe` 
                  (test200)
          (evalState (doLogic webHandle3 req') categoriesInBase)
              `shouldBe` 
                  (test200)

      it "No admin can't edit category" $ do
          let bodyReq = "{\"label\":\"Man\",\"newLabel\":\"NewMan\",\"newparent\":\"Woman\"}"
          let baseHandle' = baseHandle
          let clientAdminUser3 = Client Nothing (Just Proxy) (Just $ userLogin user3)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = clientAdminUser3 
                                     , getBody = const . pure $ bodyReq}
          (evalState (doLogic webHandle' req') categoriesInBase)
              `shouldNotBe` 
                  (test200)

  describe "EndPoint: /news" $ do

    -- pullAllNews :: Offset -> Limit -> ColumnType -> SortOrder -> Maybe Find -> [FilterItem] -> m (Either SomeException [NewsOut]),
      let req = defaultRequest
      let req' = req {rawPathInfo = "/news", queryString= [("panigate", Just "{\"offset\":0,\"limit\":10}")]}
      let logHandle = Handlers.Logger.Handle
            { Handlers.Logger.levelLogger = Handlers.Logger.Debug,
              Handlers.Logger.writeLog = \_ -> pure ()
            }

      -- type NewsOut = (Title, UTCTime, Name, [Label], Content, [URI_Image], Bool)
      let newsInBase = [("news1", read $(localtimeTemplate), "user1",["Abstract","Man"],"content1",["/images?id=1"], True)
                       ,("news2", read $(localtimeTemplate), "user2",["Abstract","Woman"],"content2",[], True)
                       ,("news3", read $(localtimeTemplate), "user3",["Abstract","Woman","Witch"],"content3",[], False)
                       ,("news4", read $(localtimeTemplate), "user1",["Abstract","Woman","Queen"],"content4",["/images?id=2","/images?id=3"], True)]

      let baseHandle  = Handlers.Base.Handle
            {
               Handlers.Base.logger = logHandle,
               Handlers.Base.sortColumnNews = DataNews,
               Handlers.Base.sortOrderNews = Descending,
               Handlers.Base.findSubString = Nothing,
               Handlers.Base.filtersNews = [],
               Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters -> get >>= pure . Right . take limit . drop offset 
                                                      } 
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               mkGoodResponse = testBuilder
                                                      }  :: Handle (State [Handlers.Base.NewsOut])

      it "All client may get list of news" $ do
--
          let baseHandle' = baseHandle
          let clientAdminUser1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let clientAdminUser2 = Client (Just Proxy) (Just Proxy) (Just $ userLogin user2)
          let clientAdminUser3 = Client Nothing (Just Proxy) (Just $ userLogin user3)
          let clientAdminUser4 = Client Nothing Nothing Nothing

          let webHandle1 = webHandle {base = baseHandle'
                                     , client = clientAdminUser1 }
          let webHandle2 = webHandle {base = baseHandle'
                                     , client = clientAdminUser2 }
          let webHandle3 = webHandle {base = baseHandle'
                                     , client = clientAdminUser3 }
          let webHandle4 = webHandle {base = baseHandle'
                                     , client = clientAdminUser4 }

          (evalState (doLogic webHandle1 req') newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ newsInBase)
          (evalState (doLogic webHandle2 req') newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ newsInBase)
          (evalState (doLogic webHandle3 req') newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ newsInBase)
          (evalState (doLogic webHandle4 req') newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ newsInBase)

      it "Client can panigate" $ do
          
          let req' = req {rawPathInfo = "/news", queryString= [("panigate", Just "{\"offset\":1,\"limit\":1}")]}
          let baseHandle' = baseHandle
          let client' = Client Nothing Nothing Nothing 

          let webHandle' = webHandle {base = baseHandle'
                                     , client = client' }

          (evalState (doLogic webHandle' req') newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ take 1 $ drop 1 newsInBase)

      it "Check default sets for news" $ do

          let baseHandle' = baseHandle {
               Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters -> 
                     pure . Right $ [(typeToText columnType, testTime, typeToText sortOrder, [], typeToText find,
                                      map typeToText filters, False)]}

          let client1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let client2 = Client Nothing Nothing Nothing
          let webHandle1 = webHandle {base = baseHandle'
                                     , client = client1}
                                     -- , getBody = const . pure $ bodyReq}

          let webHandle2 = webHandle {base = baseHandle'
                                     , client = client2}
-- data ColumnType = DataNews | AuthorNews | CategoryName | QuantityImages
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)
--
-- data SortOrder = Ascending | Descending
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)
--
-- newtype Find = Find {subString :: T.Text}
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)
--
-- data FilterItem = FilterDataAt Day | FilterDataUntil Day | FilterDataSince Day
--                   | FilterAuthorName  T.Text
--                   | FilterCategoryLabel T.Text
--                   | FilterTitleFind T.Text
--                   | FilterContentFind T.Text
--                   | FilterPublishOrAuthor (Maybe T.Text)
--   deriving stock (Eq, Show, Generic)
--   deriving anyclass (ToJSON, FromJSON)
          
          -- type NewsOut = (Title, UTCTime, Name, [Label], Content, [URI_Image], Bool)
          -- let client' = Client Nothing Nothing Nothing 

          (evalState (doLogic webHandle1 req') newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText DataNews,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

          (evalState (doLogic webHandle2 req') newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText DataNews,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),[typeToText $ FilterPublishOrAuthor Nothing],False)])

      it "Client can set sort of sort" $ do

          let req1 = req {rawPathInfo = "/news", queryString= [("sort", Just "{\"columnType\":\"DataNews\",\"sortOrder\":\"Ascending\"}")]}
          let req2 = req {rawPathInfo = "/news", queryString= [("sort", Just "{\"columnType\":\"DataNews\",\"sortOrder\":\"Descending\"}")]}
          let req3 = req {rawPathInfo = "/news", queryString= [("sort", Just "{\"columnType\":\"AuthorNews\",\"sortOrder\":\"Ascending\"}")]}
          let req4 = req {rawPathInfo = "/news", queryString= [("sort", Just "{\"columnType\":\"AuthorNews\",\"sortOrder\":\"Descending\"}")]}
          let req5 = req {rawPathInfo = "/news", queryString= [("sort", Just "{\"columnType\":\"CategoryName\",\"sortOrder\":\"Ascending\"}")]}
          let req6 = req {rawPathInfo = "/news", queryString= [("sort", Just "{\"columnType\":\"CategoryName\",\"sortOrder\":\"Descending\"}")]}
          let req7 = req {rawPathInfo = "/news", queryString= [("sort", Just "{\"columnType\":\"QuantityImages\",\"sortOrder\":\"Ascending\"}")]}
          let req8 = req {rawPathInfo = "/news", queryString= [("sort", Just "{\"columnType\":\"QuantityImages\",\"sortOrder\":\"Descending\"}")]}

          let baseHandle' = baseHandle {
               Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters -> 
                     pure . Right $ [(typeToText columnType, testTime, typeToText sortOrder, [], typeToText find,
                                      map typeToText filters, False)]}

          let client1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = client1}

          (evalState (doLogic webHandle' req1) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText DataNews,testTime,typeToText Ascending,[], typeToText (Nothing :: Maybe Find),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

          (evalState (doLogic webHandle' req2) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText DataNews,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

          (evalState (doLogic webHandle' req3) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText AuthorNews,testTime,typeToText Ascending,[], typeToText (Nothing :: Maybe Find),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

          (evalState (doLogic webHandle' req4) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText AuthorNews,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

          (evalState (doLogic webHandle' req5) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText CategoryName,testTime,typeToText Ascending,[], typeToText (Nothing :: Maybe Find),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

          (evalState (doLogic webHandle' req6) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText CategoryName,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

          (evalState (doLogic webHandle' req7) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText QuantityImages,testTime,typeToText Ascending,[], typeToText (Nothing :: Maybe Find),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

          (evalState (doLogic webHandle' req8) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText QuantityImages,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

      it "Client can search by string" $ do

          let req' = req {rawPathInfo = "/news", queryString= [("find", Just "{\"subString\":\"googleIt\"}")]}

          let baseHandle' = baseHandle {
               Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters -> 
                     pure . Right $ [(typeToText columnType, testTime, typeToText sortOrder, [], typeToText find,
                                      map typeToText filters, False)]}

          let client1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = client1}

          (evalState (doLogic webHandle' req') newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText DataNews,testTime,typeToText Descending,[], typeToText (Just $ Find "googleIt"),[typeToText . FilterPublishOrAuthor . Just . userLogin $ user1],False)])

      it "Client can filter news by date" $ do

          let req1 = req {rawPathInfo = "/news", queryString= [("filter", 
            Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataAt\"}]")]}
          let req2 = req {rawPathInfo = "/news", queryString= [("filter", 
            Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataUntil\"}]")]}
          let req3 = req {rawPathInfo = "/news", queryString= [("filter", 
            Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataSince\"}]")]}

   --                -- {\"contents\":\"user\",\"tag\":\"FilterTitleFind\"}]")]}
            -- Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataSince\"},
            --        {\"contents\":\"user\",\"tag\":\"FilterTitleFind\"}]")]}
          let baseHandle' = baseHandle {
               Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters -> 
                     pure . Right $ [(typeToText columnType, testTime, typeToText sortOrder, [], typeToText find,
                                      map typeToText filters, False)]}

          let client1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = client1}

          (evalState (doLogic webHandle' req1) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText DataNews,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),
                  (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1) :
                  [typeToText . FilterDataAt $ testDay]
                                     ,False)])

          (evalState (doLogic webHandle' req2) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText DataNews,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),
                  (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1) :
                  [typeToText . FilterDataUntil $ testDay]
                                     ,False)])

          (evalState (doLogic webHandle' req3) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText DataNews,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),
                  (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1) :
                  [typeToText . FilterDataSince $ testDay]
                                     ,False)])
          
      it "Client can filter news by data, by author name, by category label, by string in title, by string in content" $ do
--todo
          let req1 = req {rawPathInfo = "/news", 
                          queryString = [("filter",
     Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataAt\"},{\"contents\":\"Vasya\",\"tag\":\"FilterAuthorName\"},{\"contents\":\"Man\",\"tag\":\"FilterCategoryLabel\"},{\"contents\":\"a\",\"tag\":\"FilterTitleFind\"},{\"contents\":\"b\",\"tag\":\"FilterContentFind\"}]"
                                         )]}
                                                 
          let baseHandle' = baseHandle {
               Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters -> 
                     pure . Right $ [(typeToText columnType, testTime, typeToText sortOrder, [], typeToText find,
                                      map typeToText filters, False)]}

          let client1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = client1}

          (evalState (doLogic webHandle' req1) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText DataNews,testTime,typeToText Descending,[], typeToText (Nothing :: Maybe Find),
                  (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1) :
                  (typeToText . FilterDataAt $ testDay) :
                  (typeToText . FilterAuthorName $ "Vasya") :
                  (typeToText . FilterCategoryLabel $ "Man") :
                  (typeToText . FilterTitleFind $ "a") :
                  (typeToText . FilterContentFind $ "b") :
                  []
                                     ,False)])

      it "Client can filter, sort and search in one request" $ do
--todo
          let req1 = req {rawPathInfo = "/news", 
                          queryString = [("sort", Just "{\"columnType\":\"CategoryName\",\"sortOrder\":\"Ascending\"}"),("find", Just "{\"subString\":\"googleIt\"}"),("filter",
     Just "[{\"contents\":\"2023-01-01\",\"tag\":\"FilterDataAt\"},{\"contents\":\"Vasya\",\"tag\":\"FilterAuthorName\"},{\"contents\":\"Man\",\"tag\":\"FilterCategoryLabel\"},{\"contents\":\"a\",\"tag\":\"FilterTitleFind\"},{\"contents\":\"b\",\"tag\":\"FilterContentFind\"}]"
                                         )]}

          let baseHandle' = baseHandle {
               Handlers.Base.pullAllNews = \offset limit columnType sortOrder find filters -> 
                     pure . Right $ [(typeToText columnType, testTime, typeToText sortOrder, [], typeToText find,
                                      map typeToText filters, False)]}

          let client1 = Client (Just Proxy) Nothing (Just $ userLogin user1)
          let webHandle' = webHandle {base = baseHandle'
                                     , client = client1}

          (evalState (doLogic webHandle' req1) newsInBase)
              `shouldBe` 
                  (testBuilder . newsToWeb $ [(typeToText CategoryName,testTime,typeToText Ascending,[],typeToText (Just $ Find "googleIt"), 
                  (typeToText . FilterPublishOrAuthor . Just . userLogin $ user1) :
                  (typeToText . FilterDataAt $ testDay) :
                  (typeToText . FilterAuthorName $ "Vasya") :
                  (typeToText . FilterCategoryLabel $ "Man") :
                  (typeToText . FilterTitleFind $ "a") :
                  (typeToText . FilterContentFind $ "b") :
                  []
                                     ,False)])

  describe "EndPoint: /news/create" $ do
      it "Publisher can create news" $ do
          True `shouldBe` False

      it "No publisher can't create news" $ do
          True `shouldBe` False

  describe "EndPoint: /news/edit" $ do
      it "Author can edit news" $ do
          True `shouldBe` False

      it "No Author can't edit news" $ do
          True `shouldBe` False

  -- describe "EndPoint: Another" $ do
  --     let endPoints = mconcat ["/images"
  --                           ,"/news","/news/create","/news/edit"
  --                           ,"/users","/users/create"
  --                           ,"/categories","/categories/create","/categories/edit"
  --                           ]
  --     it "vse krome nashoyashix endpoint" $ do
  --       True `shouldBe`  False
testTime :: UTCTime
testTime = read $(localtimeTemplate)

testDay :: Day
testDay = fromGregorian 2023 1 1
