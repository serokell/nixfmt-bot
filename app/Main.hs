module Main where

import qualified Data.ByteString.Char8 as C8
import System.Environment (lookupEnv)
import Text.Read

import Nixfmt.Server
import Nixfmt.GitHubKey

main :: IO ()
main = do
    port <- maybe 8080 read <$> lookupEnv "PORT"
    key <- maybe mempty C8.pack <$> lookupEnv "KEY"
    putStrLn $ "Server is starting on port " ++ show port ++ " using test secret " ++ show key
    putStrLn $ "Perhaps run 'ngrok http " ++ show port ++ "' for a forwarding address"
    runServer port (server (gitHubKey $ pure key))
