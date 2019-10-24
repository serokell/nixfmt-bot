{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Nixfmt.Local.Git (processPullRequest) where

import GitHub.Endpoints.PullRequests
       ( Auth(..), Owner(..), PullRequest(..)
       , PullRequestCommit(..), Repo(..), SimpleOwner(..), URL(..))
import GitHub.Data.Name(Name(..))
import Shelly (Sh, cd, rm, rm_rf, run, shelly)
import qualified Data.Text as T
import Data.Text.Encoding as E
import Text.Megaparsec (errorBundlePretty, runParser)

import Nixfmt.Local.Parser

beginChangedBlock :: Text
beginChangedBlock = "# BEGIN NIXFMT BLOCK"

endChangedBlock :: Text
endChangedBlock = "# END NIXFMT BLOCK"

tempfileExtension :: String
tempfileExtension = ".tmp.nixfmt"

-- | Get mentioned pull request,
-- format changed pieces of text
-- and make new PR to the head branch of the PR.
processPullRequest ::
     PullRequest
  -> Maybe Auth
  -> IO (Text, Text, Name Owner, Name Repo)
processPullRequest pr mAuth = do
    let prBranch = pullRequestCommitRef . pullRequestHead $ pr
    let baseBranch = pullRequestCommitRef . pullRequestBase $ pr
    let (URL prLink) = fromMaybe (error "Can't obtain link to the repository") $
                         join $ repoCloneUrl <$> pullRequestCommitRepo (pullRequestHead pr)
    let (N rName) = fromMaybe (error "Can't obtain repository name") $
                       repoName <$> pullRequestCommitRepo (pullRequestHead pr)
    let ownerLogin = fromMaybe (error "Can't obtain owner name") $
                        simpleOwnerLogin . repoOwner <$> pullRequestCommitRepo (pullRequestHead pr)
    let (user, password) = case mAuth of
            Just (BasicAuth username passwd) -> (username, passwd)
            Just _ -> error "Only basic auth method is supported now"
            Nothing -> error "GitHub authentication info should be provided"
    let cloneURLWithAuth = "https://" <> (E.decodeUtf8 user) <> ":" <> (E.decodeUtf8 password) <> "@" <> (T.drop 8 prLink)
    _ <- (flip finally) (shelly $ rm_rf $ "/tmp/" ++ (toString rName)) $ shelly $ do
        -- clone the repo and checkout target branch.
        cd "/tmp"
        _ <- run "git" ["clone", cloneURLWithAuth]
        cd $ toString rName
        _ <- run "git" ["checkout", prBranch]
        -- get list of changed files and filter only *.nix files. Also make full paths from these names.
        files <- run "git" ["diff", "--name-only", ("origin/" <> baseBranch <> "..origin/" <> prBranch)]
        let fileList = map (\f -> "/tmp/" <> rName <> "/" <> f) $ filter (T.isSuffixOf ".nix") $ T.split (=='\n') files
        -- for each file get numbers of changed lines in format (file,[(first line, nuber of changed lines)]).
        diffLines <- mapM (processDiffs prBranch baseBranch) fileList
        --  Mark changed blocks by beginChangedBlock and endChangedBlock lines.
        --  And make a temporary copy.
        mapM_ (writeBracketsForNixFormatter ) diffLines
        -- Format these copies
        mapM_ (\(file,_) -> run "nixfmt" [(file <> ".tmp.nixfmt")]) diffLines
        -- Move formatted pieces of code back to original files and delete copies.
        mapM_ (\(file,_) -> moveFormattedCodeBack file) diffLines
        makeNewbranchAndCommit prBranch
    return (prBranch, prBranch <> "-NixFmtBot",ownerLogin , (N rName))


-- | Run "git diff" command and parse its result.
processDiffs ::
     Text
  -> Text
  -> Text
  -> Sh (Text, [(Int, Int)])
processDiffs branch baseBranch file = do
    diff <- run "git" ["diff", "-U0" , ("origin/" <> baseBranch <> "..origin/" <> branch), "--", file]
    case runParser parseDiff "" diff of
        Left e -> error $ toText $ errorBundlePretty e
        Right diffs -> return (file, diffs)

writeBracketsForNixFormatter :: (Text, [(Int,Int)]) -> Sh ()
writeBracketsForNixFormatter (_,[]) = return ()
writeBracketsForNixFormatter (toString -> file, nixLines) = do
  text <- readFile file
  let textLines = T.split (=='\n') text
  let newText = writeBracketsForNixFormatter' textLines nixLines
  writeFile (file ++ tempfileExtension) newText
  writeFile file newText

writeBracketsForNixFormatter' :: [Text] -> [(Int,Int)] -> Text
writeBracketsForNixFormatter' nixLines [] = T.intercalate "\n" nixLines
writeBracketsForNixFormatter' nixLines ((line, lineEnd):xs) =
  let newNixLines = insertNum (line - 1) beginChangedBlock $
                      insertNum (line + lineEnd - 1) endChangedBlock $
                        nixLines
      newLineNums = map (\(ln,lns) -> (ln+2,lns)) xs

      insertNum :: Int -> a -> [a] -> [a]
      insertNum 0 a bs = a : bs
      insertNum _ _ [] = error "Too short list to insert a line"
      insertNum n a (b:bs) = b : (insertNum (n-1) a bs)
  in writeBracketsForNixFormatter' newNixLines newLineNums

moveFormattedCodeBack :: Text -> Sh ()
moveFormattedCodeBack (toString -> file) = do
    let tmpFile = file ++ tempfileExtension
    original <- readFile file
    formatted <- readFile tmpFile
    let changedTexts =
          case runParser (parseFormatted beginChangedBlock endChangedBlock) "" formatted of
            Left e -> error $ toText $ errorBundlePretty e
            Right txts -> txts
    let replaced =
          case runParser (replaceOldFragments beginChangedBlock endChangedBlock changedTexts) "" original of
            Left e -> error $ toText $ errorBundlePretty e
            Right txt -> txt
    writeFile file replaced
    rm tmpFile

makeNewbranchAndCommit :: Text -> Sh ()
makeNewbranchAndCommit brName = do
    let newBranchName = brName <> "-NixFmtBot"
    _ <- run "git" ["checkout", "-b", newBranchName]
    _ <- run "git" ["add", "."]
    _ <- run "git" ["commit", "-m", "Nix formatter bot changes.\n\nFiles were changed by Nix formatter bot."]
    _ <- run "git" ["push", "--set-upstream", "origin", newBranchName]
    return ()
