module Nixfmt.GitHubKey
       ( GitHubKey
       , gitHubKey
       ) where

import Universum

import Servant.Server (HasContextEntry (..), Context (..))
import qualified Servant.GitHub.Webhook as SGH (GitHubKey, gitHubKey)
import qualified Data.ByteString  as BS

newtype GitHubKey = GitHubKey (forall result . SGH.GitHubKey result)

gitHubKey :: IO BS.ByteString -> GitHubKey
gitHubKey k = GitHubKey (SGH.gitHubKey k)

instance HasContextEntry '[GitHubKey] (SGH.GitHubKey result) where
    getContextEntry (GitHubKey x :. _) = x
