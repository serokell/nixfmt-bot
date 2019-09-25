module Nixfmt.WebhookHandlers
       ( nixfmtHandlers
       ) where

import GitHub.Data.Webhooks.Events  (IssueCommentEvent (..))
import GitHub.Data.Webhooks.Payload  (HookIssueComment (..))
import Servant.GitHub.Webhook (RepoWebhookEvent (..))
import Servant.Server (Handler)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServer, genericServer)

import Nixfmt.ServerAPI (NixfmtWebhooks (..))
import Nixfmt.GitHubEvent

type NixfmtHandlers = ToServant NixfmtWebhooks AsServer

nixfmtHandlers :: NixfmtHandlers
nixfmtHandlers = genericServer NixfmtWebhooks
    { nwPostNewComment = onIssueCommentEvent
    }

onIssueCommentEvent :: RepoWebhookEvent -> ((), IssueCommentEvent) -> Handler ()
onIssueCommentEvent _ (_, event@IssueCommentEvent{..})
    | isPullRequestEvent event
    && isBotMention (whIssueCommentBody evIssueCommentPayload)
    && isCreatedEvent event =
        putTextLn $ "Issue comment text: " <> whIssueCommentBody evIssueCommentPayload
    | otherwise  = putTextLn "Event has been skipped"