{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.GitHubRequests
       ( getPRInfo
       , makeNewPR
       ) where

import GitHub.Endpoints.PullRequests
  ( Auth(..), CreatePullRequest(..), Error(..),IssueNumber(..)
  , Owner(..), PullRequest(..), Repo(..), URL(..), createPullRequest, pullRequest')
import GitHub.Data.Name (Name(..))

import Text.Megaparsec hiding (many)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, string)

type Parser = Parsec Void Text
type UserName = Text
type RepoName = Text

pRTitle :: Text
pRTitle = "Nix formatter bot"

pRBody :: Text
pRBody = "This PR had been made automatically by nixfmt-bot. It contains formatted diffs from the base pull request."

getPRInfo :: Maybe Auth -> URL -> IO (Either Error PullRequest)
getPRInfo mAuth (URL url) = case runParser parsePRUrl "" url of
    Left errorBundle -> return . Left . ParseError . toText . errorBundlePretty $ errorBundle
    Right (userName, repoName, prNumber) ->
      pullRequest' mAuth (N userName) (N repoName) (IssueNumber prNumber)

makeNewPR ::
     Maybe Auth
  -> Text
  -> Text
  -> Name Owner
  -> Name Repo
  -> IO (Either Error PullRequest)
makeNewPR mAuth baseBranch newBranch owner repo =
    let createPR = CreatePullRequest pRTitle pRBody newBranch baseBranch
    in case mAuth of
            Just auth -> createPullRequest auth owner repo createPR
            Nothing   -> error "Only basic auth method is supported now"

parsePRUrl :: Parser (RepoName, UserName, Int)
parsePRUrl = do
    _ <- string "https://github.com/"
    userName <- many (noneOf ("/":: String ))
    _ <- char '/'
    repoName <- many (noneOf ("/" :: String))
    _ <- string "/pull/"
    numberOfPR <- decimal
    return (toText userName, toText repoName, numberOfPR)

