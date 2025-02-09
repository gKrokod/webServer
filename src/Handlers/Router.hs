{-# LANGUAGE DataKinds #-}

module Handlers.Router (Client (..), doAuthorization, doLogic) where

import Control.Monad (when)
import Control.Monad.Except (ExceptT (..), runExceptT)
import Data.Bool (bool)
import qualified Data.ByteString as B
import Data.Maybe (isNothing)
import Data.Proxy (Proxy (..))
import qualified Data.Text as T
import Handlers.Database.Api (getPrivilege, getResultValid)
import qualified Handlers.Logger
import Handlers.Web.Api (endPointCategories, endPointImages, endPointNews, endPointUsers)
import Handlers.Web.Base (Handle (..))
import qualified Handlers.Web.News
import Handlers.Database.Auth (Client (..))
import Network.Wai (Request, Response, rawPathInfo, requestHeaders)
import Schema (IsValidPassword (..))
import Types (Login (..), PasswordUser (..))
import Web.Query (headersToLoginAndPassword)

doAuthorization :: (Monad m) => Handle m -> Request -> m (Either Response (Handle m))
doAuthorization h req = do
  let logHandle = logger h
      newsHandle = news h
  userRole <- getClient h req
  case userRole of
    Left e -> do
      Handlers.Logger.logMessage logHandle Handlers.Logger.Error e
      pure . Left $ Handlers.Web.News.response403 newsHandle
    Right clientRole -> do
      let h' = h {client = clientRole}
      pure $ Right h'

doLogic :: (Monad m) => Handle m -> Request -> m Response
doLogic h req = do
  let logHandle = logger h
      newsHandle = news h
  case rawPathInfo req of
    path
      | B.isPrefixOf "/news" path -> endPointNews h req
      | B.isPrefixOf "/users" path -> endPointUsers h req
      | B.isPrefixOf "/categories" path -> endPointCategories h req
      | B.isPrefixOf "/images" path -> endPointImages h req
      | otherwise -> do
          Handlers.Logger.logMessage logHandle Handlers.Logger.Warning "End point not found"
          pure $ Handlers.Web.News.response404 newsHandle

getClient :: (Monad m) => Handle m -> Request -> m (Either T.Text Client)
getClient h req = do
  let baseHandle = auth h
  -- let baseHandle = base h
      secureData = headersToLoginAndPassword . requestHeaders $ req
  when (isNothing secureData) (Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "Request don't have Login and Password")
  runExceptT $ do
    case secureData of
      Nothing ->
        pure $
          Client
            { clientAdminToken = Nothing,
              clientPublisherToken = Nothing,
              author = Nothing
            }
      Just (login_, password_) -> do
        (isAdmin_, isPublisher_) <- ExceptT $ getPrivilege baseHandle (MkLogin login_)
        valid <- ExceptT $ getResultValid baseHandle (MkLogin login_) (MkPasswordUser password_)
        case valid of
          NotValid ->
            pure $
              Client
                { clientAdminToken = Nothing,
                  clientPublisherToken = Nothing,
                  author = Nothing
                }
          Valid ->
            pure $
              Client
                { clientAdminToken = bool Nothing (Just Proxy) isAdmin_,
                  clientPublisherToken = bool Nothing (Just Proxy) isPublisher_,
                  author = Just . MkLogin $ login_
                }
