module Nixfmt.GitHubEvent
       ( isIssueEvent
       , isPullRequestEvent
       , isBotMention
       , isCreatedEvent
       ) where

import Universum
import qualified Data.Text as T
import GitHub.Data.Webhooks.Events  (IssueCommentEvent (..), IssueCommentEventAction (..))
import GitHub.Data.Webhooks.Payload (getUrl, HookIssue (..))

isIssueEvent :: IssueCommentEvent -> Bool
isIssueEvent = ("/issues/" `T.isInfixOf`) . getUrl . whIssueHtmlUrl . evIssueCommentIssue

isPullRequestEvent :: IssueCommentEvent -> Bool
isPullRequestEvent = ("/pull/" `T.isInfixOf`) . getUrl . whIssueHtmlUrl . evIssueCommentIssue

isBotMention :: Text -> Bool
isBotMention = ("@nixfmt" `T.isInfixOf`)

isCreatedEvent :: IssueCommentEvent -> Bool
isCreatedEvent ev = evIssueCommentAction ev == IssueCommentCreatedAction
