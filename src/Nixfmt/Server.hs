{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.Server
       ( server
       , runServer
       ) where

import Universum

import Network.Wai (Application)
import Servant.Server (Context (..), serveWithContext)
import Network.Wai.Handler.Warp (run)

import GitHub.Auth (Auth(..))

import Nixfmt.GitHubKey
import Nixfmt.ServerAPI
import Nixfmt.WebhookHandlers

--newtype GitHubAuth = OAuth BS.ByteString

runServer :: MonadIO m => Int -> Application -> m a
runServer port app = do
  liftIO $ run port app
  return $ error "Server terminated early"


server :: GitHubKey -> Maybe Auth -> Application
server key mAuth = serveWithContext (Proxy :: Proxy NixfmtWebhooksAPI) (key :. EmptyContext) (nixfmtHandlers mAuth)
