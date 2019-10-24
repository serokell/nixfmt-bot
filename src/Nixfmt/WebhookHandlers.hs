module Nixfmt.WebhookHandlers
       ( nixfmtHandlers
       ) where

import GitHub.Data.Webhooks.Events  (IssueCommentEvent (..))
import GitHub.Data.Webhooks.Payload  (HookIssueComment (..), whIssueHtmlUrl, getUrl) -- whIssueHtmlUrl) --, getUrl)
import Servant.GitHub.Webhook (RepoWebhookEvent (..))
import Servant.Server (Handler)
import Servant.API.Generic (ToServant)
import Servant.Server.Generic (AsServer, genericServer)

import GitHub.Auth (Auth(..))
import GitHub.Data.URL (URL(..))
import GitHub.Data.Definitions (IssueNumber(..))
import GitHub.Data.PullRequests (pullRequestNumber)

import Nixfmt.ServerAPI (NixfmtWebhooks (..))
import Nixfmt.GitHubEvent
import Nixfmt.GitHubRequests
import Nixfmt.Local.Git

import Control.Concurrent.Async

type NixfmtHandlers = ToServant NixfmtWebhooks AsServer

nixfmtHandlers :: (Maybe Auth) -> NixfmtHandlers
nixfmtHandlers mAuth = genericServer NixfmtWebhooks
    { nwPostNewComment = (onIssueCommentEvent mAuth)
    }

onIssueCommentEvent :: Maybe Auth -> RepoWebhookEvent -> ((), IssueCommentEvent) -> Handler ()
onIssueCommentEvent mAuth _ (_, event@IssueCommentEvent{..})
    | isPullRequestEvent event
    && isBotMention (whIssueCommentBody evIssueCommentPayload)
    && isCreatedEvent event = do
--        let auth = BasicAuth "Haskell-mouse" "dab611558678d0eebcc85d7730f1568830f5dafc"
        putTextLn $ "Issue PR Url: " <> getUrl (whIssueHtmlUrl evIssueCommentIssue)
        let url = getUrl (whIssueHtmlUrl evIssueCommentIssue)
        ePullReq <- liftIO $ getPRInfo mAuth (URL url) -- (whIssueHtmlUrl evIssueCommentIssue)
        _ <- liftIO $ async $ do
                (baseBranch, newBranch, owner, repo) <- case ePullReq of
                        Left err -> error $ show err
                        Right pullReq -> processPullRequest pullReq mAuth
                makePRResult <- makeNewPR mAuth baseBranch newBranch owner repo
                case makePRResult of
                       Left err -> putStrLn @String $ show err
                       Right pr -> let (IssueNumber prNumber) = pullRequestNumber pr
                                   in putTextLn $ "Made pull request with number " <> (show prNumber)
        return ()
    | otherwise  = putTextLn "Event has been skipped"
