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
import Handlers.Database.Auth (Client (..))
import qualified Handlers.Logger
import Handlers.Web.Api (endPointCategories, endPointImages, endPointNews, endPointUsers)
import Handlers.Web.Base (Handle (..))
import Network.Wai (Request, Response, rawPathInfo, requestHeaders)
import Schema (IsValidPassword (..))
import Types (Login (..), PasswordUser (..))
import Web.Query (headersToLoginAndPassword)
import qualified Web.Utils as WU

doAuthorization :: (Monad m) => Handle m -> Request -> m (Either Response (Handle m))
doAuthorization h req = do
  userRole <- getClient h req
  case userRole of
    Left e -> do
      Handlers.Logger.logMessage (logger h) Handlers.Logger.Error e
      pure . Left $ WU.response403
    Right clientRole -> do
      let h' = h {client = clientRole}
      pure $ Right h'

doLogic :: (Monad m) => Handle m -> Request -> m Response
doLogic h req = do
  case rawPathInfo req of
    path
      | B.isPrefixOf "/news" path -> endPointNews h req
      | B.isPrefixOf "/users" path -> endPointUsers h req
      | B.isPrefixOf "/categories" path -> endPointCategories h req
      | B.isPrefixOf "/images" path -> endPointImages h req
      | otherwise -> do
          Handlers.Logger.logMessage (logger h) Handlers.Logger.Warning "End point not found"
          pure WU.response404

getClient :: (Monad m) => Handle m -> Request -> m (Either T.Text Client)
getClient h req = do
  let baseHandle = auth h
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
