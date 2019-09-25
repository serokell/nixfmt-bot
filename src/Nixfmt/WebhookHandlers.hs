module Nixfmt.WebhookHandlers
       ( nixfmtHandlers
       ) where

import GitHub.Data.Webhooks.Events  (IssueCommentEvent (..))
import Servant.GitHub.Webhook (RepoWebhookEvent (..))
import Servant.Server (Handler)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServer, genericServer)

import Nixfmt.ServerAPI (NixfmtWebhooks (..))

type NixfmtHandlers = ToServant NixfmtWebhooks AsServer

nixfmtHandlers :: NixfmtHandlers
nixfmtHandlers = genericServer NixfmtWebhooks
    { nwPostNewComment = onIssueCommentEvent
    }

onIssueCommentEvent :: RepoWebhookEvent -> ((), IssueCommentEvent) -> Handler ()
onIssueCommentEvent _ (_, event) = putTextLn $ "Event: " <> show event