module Nixfmt.Server
       ( server
       , runServer
       ) where

import Network.Wai (Application)
import Servant.Server (Context (..), serveWithContext)
import Network.Wai.Handler.Warp (run)

import Nixfmt.GitHubKey
import Nixfmt.ServerAPI
import Nixfmt.WebhookHandlers

runServer :: MonadIO m => Int -> Application -> m a
runServer port app = do
  liftIO $ run port app
  return $ error "Server terminated early"


server :: GitHubKey -> Application
server key = serveWithContext
    (Proxy :: Proxy NixfmtWebhooksAPI)
    (key :. EmptyContext)
    nixfmtHandlers