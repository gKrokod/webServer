{-# LANGUAGE DataKinds #-}

module Handlers.Router (Client (..), doAuthorization, doLogic) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Control.Monad.Trans (lift)
import Data.Bool (bool)
import qualified Data.ByteString as B
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (getPrivilege, getResultValid)
import qualified Handlers.Logger
import Handlers.Web.Api (endPointCategories, endPointImages, endPointNews, endPointUsers)
import Handlers.Web.Base (Client (..), Handle (..))
import Network.Wai (Request, Response, rawPathInfo, requestHeaders)
import Schema (IsValidPassword (..))
import Types (Login (..), PasswordUser (..))
import Web.WebType (headersToLoginAndPassword)

doAuthorization :: (Monad m) => Handle m -> Request -> m (Either Response (Handle m))
doAuthorization h req = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "do Authorization"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get request"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show req)
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get headers"
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug (T.pack $ show $ requestHeaders req)
  userRole <- getClient h req
  case userRole of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure (Left $ response404 h)
    Right clientRole -> do
      let h' = h {client = clientRole}
      pure $ Right h'

doLogic :: (Monad m) => Handle m -> Request -> m Response
doLogic h req = do
  let logHandle = logger h
  Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "run doLogic"
  case rawPathInfo req of
    path
      | B.isPrefixOf "/news" path -> endPointNews h req
      | B.isPrefixOf "/users" path -> endPointUsers h req
      | B.isPrefixOf "/categories" path -> endPointCategories h req
      | B.isPrefixOf "/images" path -> endPointImages h req
      | otherwise -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
          pure (response404 h) 

getClient :: (Monad m) => Handle m -> Request -> m (Either T.Text Client)
getClient h req = do
  let logHandle = logger h
      baseHandle = base h
      secureData = headersToLoginAndPassword . requestHeaders $ req
  when (isNothing secureData) (Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "Request don't have Login and Password")
  runExceptT $ do
    case secureData of
      Nothing -> pure $ Client {
        clientAdminToken = Nothing,
        clientPublisherToken = Nothing,
        author = Nothing} 
      Just (login_, password_) -> do
        (isAdmin_, isPublisher_) <- ExceptT $ getPrivilege baseHandle (MkLogin login_)
        valid <- ExceptT $ getResultValid baseHandle (MkLogin login_) (MkPasswordUser password_)
        case valid of
          NotValid -> do
            lift $ Handlers.Logger.logMessage (logger h) Handlers.Logger.Debug "Password is incorrect"
            pure $ Client {
              clientAdminToken = Nothing,
              clientPublisherToken = Nothing,
              author = Nothing}
          Valid -> do
            lift $ Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "Password is correct"
            lift $ Handlers.Logger.logMessage logHandle Handlers.Logger.Debug "get privilege"
            pure $ Client {
              clientAdminToken = bool Nothing (Just Proxy) isAdmin_,
              clientPublisherToken = bool Nothing (Just Proxy) isPublisher_,
              author = Just . MkLogin $ login_}

