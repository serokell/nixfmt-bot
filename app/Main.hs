module Main where

import qualified Data.ByteString.Char8 as C8
import System.Environment (lookupEnv)
import Text.Read (read)

import Nixfmt.Server
import Nixfmt.GitHubKey

import GitHub.Auth (Auth(..))

main :: IO ()
main = do
    port   <- maybe 8080 read <$> lookupEnv "PORT"
    key    <- maybe mempty C8.pack <$> lookupEnv "KEY"
    login  <- lookupEnv "GITHUB_LOGIN"
    passwd <- lookupEnv "GITHUB_PASSWORD"
    let mAuth = BasicAuth <$> (C8.pack <$> login) <*> (C8.pack <$> passwd)
    putStrLn $ "Server is starting on port " ++ show port ++ " using test secret " ++ show key
    putStrLn $ "Perhaps run 'ngrok http " ++ show port ++ "' for a forwarding address"
    runServer port (server (gitHubKey $ pure key) mAuth)
