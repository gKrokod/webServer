{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
module Handlers.WebLogicSpec (spec) where

import Scheme
import Base.FillTables (user1, user2, user3, cat1,cat2,cat3,cat4,cat5,cat6,cat7,cat9,cat8, news1,news2,news3,news4)
import Test.Hspec
import Handlers.WebLogic (getClient)
import qualified Handlers.Logger 
import qualified Handlers.Base
-- import qualified Logger 
import Control.Monad.State
import Test.QuickCheck
import Data.Maybe
import Data.Either (isLeft)

spec :: Spec
spec = do
  describe "Made Client with right privilege" $ do
      let usersInBase = [user1,user2,user3]

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
                                                            then Just user else Nothing) $ users)
               -- validPassword = \login password -> pure $ Right True
                                                      } -- :: Handle (State [User])
      let webHandle  = Handle
            {
               logger = logHandle,
               base = baseHandle,
               mkGoodResponse = \bulder -> undefined
                                                      }  :: Handle (State [User])
      it "haha" $ do
          let baseHandle' = baseHandle {Handlers.Base.validPassword = \login password -> pure $ Right True}
          let webHandle' = webHandle {base = baseHandle'}

          isLeft (evalState (getClient webHandle' undefined) usersInBase)
             `shouldBe` 
                     True
        -- True `shouldBe` True

  describe "part 2" $ do

      it "haha" $ do
        True `shouldBe`  False

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
