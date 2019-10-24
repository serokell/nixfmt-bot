module Nixfmt.GitHubEvent
       ( isIssueEvent
       , isPullRequestEvent
       , isBotMention
       , isCreatedEvent
       ) where

import qualified Data.Text as T
import GitHub.Data.Webhooks.Events  (IssueCommentEvent (..), IssueCommentEventAction (..))
import GitHub.Data.Webhooks.Payload (getUrl, HookIssue (..))

isIssueEvent :: IssueCommentEvent -> Bool
isIssueEvent = endsWith "issues" . getUrl . whIssueHtmlUrl . evIssueCommentIssue

isPullRequestEvent :: IssueCommentEvent -> Bool
isPullRequestEvent = endsWith "pull" . getUrl . whIssueHtmlUrl . evIssueCommentIssue

isBotMention :: Text -> Bool
isBotMention = ("@nixfmt" == )

isCreatedEvent :: IssueCommentEvent -> Bool
isCreatedEvent ev = evIssueCommentAction ev == IssueCommentCreatedAction

endsWith :: Text -> Text -> Bool
endsWith sub s = sub `T.isSuffixOf` T.init (T.dropWhileEnd (/= '/') s)
