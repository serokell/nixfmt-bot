{-# LANGUAGE OverloadedStrings #-}

module Nixfmt.GitHubRequests
       ( getPRInfo
       , makeNewPR
       ) where

import Universum

import GitHub.Endpoints.PullRequests
  ( Auth(..), CreatePullRequest(..), Error(..),IssueNumber(..)
  , Owner(..), PullRequest(..), Repo(..), URL(..), createPullRequest, pullRequest')
import GitHub.Data.Name (Name(..))

import Text.Megaparsec hiding (many)
import Text.Megaparsec.Char.Lexer (decimal)
import Text.Megaparsec.Char (char, string)

import qualified Data.ByteString.Char8 as C8

type Parser = Parsec Void Text
type UserName = Text
type RepoName = Text

pRTitle :: Text
pRTitle = "Format code automatically"

pRBody :: Text
pRBody = ""

getPRInfo :: Maybe Auth -> URL -> IO (Either Error PullRequest)
getPRInfo mAuth (URL url) = case runParser parsePRUrl "" url of
    Left errorBundle -> return . Left . ParseError . toText . errorBundlePretty $ errorBundle
    Right (userName, repoName, prNumber) ->
      pullRequest' mAuth (N userName) (N repoName) (IssueNumber prNumber)

makeNewPR ::
     Maybe Auth
  -> Text
  -> Name Owner
  -> Name Repo
  -> IO (Either Error PullRequest)
makeNewPR mAuth baseBranch owner repo = case mAuth of
            Just auth@(BasicAuth botName _) ->
              let newBranchName = (toText $ C8.unpack botName) <> ":" <> baseBranch
                  createPR = CreatePullRequest pRTitle pRBody newBranchName baseBranch
              in  createPullRequest auth owner repo createPR
            Just _    -> error "Only basic auth method is supported now"
            Nothing   -> error "Credentials should be provided"

parsePRUrl :: Parser (RepoName, UserName, Int)
parsePRUrl = do
    _ <- string "https://github.com/"
    userName <- many (noneOf ("/":: String ))
    _ <- char '/'
    repoName <- many (noneOf ("/" :: String))
    _ <- string "/pull/"
    numberOfPR <- decimal
    return (toText userName, toText repoName, numberOfPR)

