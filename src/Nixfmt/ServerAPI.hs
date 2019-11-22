module Nixfmt.ServerAPI
       ( NixfmtWebhooksAPI
       , NixfmtWebhooks (..)
       ) where

import Universum

import GitHub.Data.Webhooks.Events (IssueCommentEvent (..))
import Servant.API.Generic ((:-), AsApi, ToServant)
import Servant.API (JSON, Post, (:>))
import Servant.GitHub.Webhook (GitHubEvent, GitHubSignedReqBody, RepoWebhookEvent (..))

data NixfmtWebhooks route = NixfmtWebhooks
    { nwPostNewComment :: route
      :- GitHubEvent '[ 'WebhookIssueCommentEvent ]
      :> GitHubSignedReqBody '[JSON] IssueCommentEvent
      :> Post '[JSON] ()
    } deriving (Generic)

type NixfmtWebhooksAPI = ToServant NixfmtWebhooks AsApi
