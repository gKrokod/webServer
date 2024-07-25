{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Handlers.WebLogicSpec (spec) where

import Scheme
import Base.FillTables (user1, user2, user3, cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat9,cat8, news1,news2,news3,news4)
import Test.Hspec
import Handlers.WebLogic
import qualified Handlers.Logger 
import qualified Handlers.Base
-- import qualified Logger 
import Control.Monad.State
import Test.QuickCheck
import Data.Maybe
import Data.Either (isLeft)
import Network.Wai (defaultRequest, Request, Response, rawPathInfo, queryString, requestHeaders)
import Network.Wai (Request, Response,getRequestBodyChunk, responseBuilder)
import Network.HTTP.Types (notFound404, status200)

test404 :: Response
test404 = responseBuilder notFound404 [] "Not ok. status 404\n" 

test200 :: Response
test200 = responseBuilder status200 [] "All ok. status 200\n" 

spec :: Spec
spec = do
  describe "Create User" $ do

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
               Handlers.Base.putUser = \name login pass time admin publish -> pure $ Right Handlers.Base.Put
                                                      } 
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               getBody = pure "{\"isAdmin\":true,\"isPublisher\":true,\"login\":\"Дагер\",\"name\":\"Петр\",\"password\":\"qwerty\"}",
               mkGoodResponse = \bulder -> undefined
                                                      }  :: Handle (State [User])
      it "Good Request" $ do
          let baseHandle' = baseHandle
          -- let baseHandle' = baseHandle {Handlers.Base.validPassword = \login password -> pure $ Right True}
          let webHandle' = webHandle {base = baseHandle'}

          (evalState (createUser undefined webHandle' undefined) usersInBase)
              `shouldBe` 
                      test200

  describe "part 2" $ do

      it "haha" $ do
        True `shouldBe`  False

-- createUser :: (Monad m) => Proxy 'AdminRole -> Handle m -> Request -> m Response -- for Admin
-- -- createUser :: (Monad m) => Handle m -> Proxy Admin -> Request -> m (Response) -- for Admin
-- createUser _ h req = do
--   let logHandle = logger h 
--   let baseHandle = base h 
--   Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "create User WEB"
--   body <- webToUser <$> getBody h req -- :: (Either String UserFromWeb)
--   case body of
--     Left e -> do 
--       Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "fail decode User WEB"
--       Handlers.Logger.logMessage logHandle Handlers.Logger.Warning (T.pack e)  
--       pure (response404 h) -- "Not ok. User cannot be created. Status 404\n"
--     Right (UserFromWeb name_ login_ password_ admin_ publisher_) -> do
--       Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Try to create user WEB"
--       --todo new password?
--       tryCreateUser <- Handlers.Base.createUserBase baseHandle name_ login_ password_ admin_ publisher_
--       case tryCreateUser of
--         Right _ -> do
--           Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Create User success WEB"
--           pure (response200 h)
--         Left e -> do
--           Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e  
--           pure $ response404 h -- "Not ok. 

-- data Handle m = Handle
--   { logger :: Handlers.Logger.Handle m,
--    -- logMessage 
--     base :: Handlers.Base.Handle m,
--     client :: Client,
--     getBody :: Request -> m B.ByteString,
--     response404 :: Response,
--     response200 :: Response,
--     mkGoodResponse :: Builder -> Response,
--     mkResponseForImage :: Image -> Response,
--     response404WithImage :: Response
--   }
-- getClient :: (Monad m) => Handle m -> Request -> m (Either T.Text Client)
-- getClient h req = do
--   let logHandle = logger h 
--   let baseHandle = base h 
--   let secureData = headersToLoginAndPassword . requestHeaders $ req
--   when (isNothing secureData) (Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "Request don't have Login and Password")
--   runExceptT $ do
--     case secureData of
--       Nothing -> pure $ Client Nothing Nothing Nothing
--       Just (login_, password_) -> do --
--         (isAdmin_, isPublisher_) <- ExceptT $ Handlers.Base.getPrivilege baseHandle login_ 
--         valid <- ExceptT $ Handlers.Base.getResultValid baseHandle login_ password_
--         case valid of 
--           NotValid -> do 
--             lift $ Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "Password is incorrect"
--             pure $ Client Nothing Nothing Nothing
--           Valid -> do
--             lift $ Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Password is correct"
--             lift $ Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get privilege" 
--             pure $ Client (bool Nothing (Just Proxy) isAdmin_) 
--                           (bool Nothing (Just Proxy) isPublisher_) 
--  
