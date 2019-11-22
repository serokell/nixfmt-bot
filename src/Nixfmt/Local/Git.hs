{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ExtendedDefaultRules #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}


module Nixfmt.Local.Git (FailMsg(..), processPullRequest) where

import Universum

import GitHub.Endpoints.PullRequests
       ( Auth(..), Owner(..), PullRequest(..)
       , PullRequestCommit(..), Repo(..), SimpleOwner(..), URL(..))
import GitHub.Endpoints.Repos (deleteRepo)
import GitHub.Endpoints.Repos.Forks (forkRepo)
import GitHub.Data.Name(Name(..))
import Shelly (Sh, cd, rm, rm_rf, run, shelly, silently)
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

data FailMsg = NothingWasChanged

-- | Get mentioned pull request,
-- format changed pieces of text
-- and make new PR to the head branch of the PR.
processPullRequest ::
     PullRequest
  -> Maybe Auth
  -> IO (Either FailMsg (Text, Name Owner, Name Repo))
processPullRequest pr mAuth = do
    let originalRepo = fromMaybe (error "Can't obtain the original repository") $
                          pullRequestCommitRepo . pullRequestHead $ pr
    let prRepo       = fromMaybe (error "Can't obtain the repository") $
                          pullRequestCommitRepo . pullRequestBase $ pr
    let theSameRepo  = originalRepo == prRepo
    let (URL baseRepoLink) = fromMaybe (error "Can't obtain link to the original repository") $
                                repoCloneUrl $ prRepo
    let mOriginalRepoURL = if theSameRepo then Nothing else Just baseRepoLink
    let prBranch = pullRequestCommitRef . pullRequestHead $ pr
    let baseBranch = pullRequestCommitRef . pullRequestBase $ pr
    let (N rName) = fromMaybe (error "Can't obtain repository name") $
                       repoName <$> pullRequestCommitRepo (pullRequestHead pr)
    let ownerLogin = fromMaybe (error "Can't obtain owner name") $
                        simpleOwnerLogin . repoOwner <$> pullRequestCommitRepo (pullRequestHead pr)
    let (user, password) = case mAuth of
            Just (BasicAuth username passwd) -> (username, passwd)
            Just _ -> error "Only basic auth method is supported now"
            Nothing -> error "GitHub authentication info should be provided"
    -- fork the repo with PR
    newRepoE <- forkRepo (BasicAuth user password) ownerLogin (N rName) Nothing
    let (N l) = ownerLogin
    putTextLn $ l <> " : " <> rName
    let newRepo = case newRepoE of
                     Right repo -> repo
                     Left err   -> error $ toText $ show err
    let (URL newRepoURL) = fromMaybe (error "Can't obtain the forked repo URL!") $ repoCloneUrl newRepo
    let cloneURLWithAuth = "https://" <> (E.decodeUtf8 user)
                             <> ":" <> (E.decodeUtf8 password) <> "@"
                             <> (T.drop 8 newRepoURL)
    result <- (flip onException) (deleteRepo (BasicAuth user password) (N $ E.decodeUtf8 user) (N rName)) $
         (flip finally) (shelly $ rm_rf $ "/tmp/" ++ (toString rName)) $ shelly $ do
            -- clone the forked repo and checkout target branch.
            cd "/tmp"
            _ <- run "git" ["clone", cloneURLWithAuth]
            cd $ toString rName
            _ <- run "git" ["checkout", prBranch]
            -- get list of changed files and filter only *.nix files.
            -- Also make full paths from these names.
            (mRepo, fileList) <- processFileList
               prBranch baseBranch ("/tmp/" <> rName <> "/") mOriginalRepoURL
            -- if there is nothing to do then delete forked repo
            if null fileList
              then do
                _ <- liftIO $ deleteRepo (BasicAuth user password) (N $ E.decodeUtf8 user) (N rName)
                return $ Left NothingWasChanged
              else do
                   let baseRepo = fromMaybe "origin" mRepo
                   -- for each file get numbers of changed lines in format
                   -- (file,[(first line, nuber of changed lines)]).
                   diffLines <- mapM (processDiffs prBranch baseBranch baseRepo) fileList
                   -- Check every file if number of added lines is more or equal
                   --  than 0.5 * number of lines in the file.
                   -- If it is true, then format the whole file in place and remove it from list
                   checkedDiffLines <- catMaybes <$> mapM checkAndFormatWholeFileInPlace diffLines
                   -- Mark changed blocks by beginChangedBlock and endChangedBlock lines.
                   -- And make a temporary copy.
                   mapM_ writeBracketsForNixFormatter checkedDiffLines
                   -- Format these copies
                   mapM_ (\(file,_) -> run "nixfmt" [file <> ".tmp.nixfmt"]) checkedDiffLines
                   -- Move formatted pieces of code back to original files and delete copies.
                   mapM_ (\(file,_) -> moveFormattedCodeBack file) checkedDiffLines
                   -- Make a commit
                   Right <$> commit
    case result of
      Right () -> return $ Right (prBranch, ownerLogin , (N rName))
      Left _ -> return $ Left NothingWasChanged

processFileList ::
    Text
 -> Text
 -> Text
 -> Maybe Text
 -> Sh (Maybe Text, [Text])
processFileList branch baseBranch path mOldRepoLink = do
  fileStatus <- case mOldRepoLink of
    Nothing   ->
      run "git" ["diff", "--name-status", ("origin/" <> baseBranch <> "..origin/" <> branch)]
    Just link -> do
                   _ <- run "git" ["remote", "add", "repo2", link]
                   _ <- run "git" ["fetch", "repo2"]
                   run "git"
                     ["diff", "--name-status", ("repo2/" <> baseBranch <> "..origin/" <> branch)]
  case runParser (parseFiles path) "" fileStatus of
      Left e -> error $ toText $ errorBundlePretty e
      Right files ->
        case mOldRepoLink of
          Nothing -> return $ (Nothing, filter (T.isSuffixOf ".nix") files)
          Just _  -> return $ (Just "repo2", filter (T.isSuffixOf ".nix") files)

-- | Run "git diff" command and parse its result.
processDiffs ::
     Text
  -> Text
  -> Text
  -> Text
  -> Sh (Text, [(Int, Int)])
processDiffs branch baseBranch repo file = do
    diff <- silently $
      run "git" ["diff", "-U0" , (repo <> "/" <> baseBranch <> "..origin/" <> branch), "--", file]
    case runParser parseDiff "" diff of
        Left e -> error $ toText $ errorBundlePretty e
        Right diffs -> return (file, diffs)

checkAndFormatWholeFileInPlace :: (Text, [(Int,Int)]) -> Sh (Maybe (Text, [(Int,Int)]))
checkAndFormatWholeFileInPlace f@(tf@(toString -> file), changedLines) = do
   liftIO $ putTextLn $ "Checking file " <> tf
   numberOfLinesInFile <- length . T.split (=='\n') <$> readFile file
   let numberOfChangedLines = sum $ snd <$> (changedLines)
   liftIO $ putTextLn $ (show numberOfChangedLines) <> " .. " <> (show numberOfLinesInFile)
   if numberOfChangedLines >= numberOfLinesInFile `div` 2
   then do
      liftIO $ putTextLn $ "Formatting the whole file " <> tf
      _ <- run "nixfmt" [toText file]
      return Nothing
   else return $ Just f


writeBracketsForNixFormatter :: (Text, [(Int,Int)]) -> Sh ()
writeBracketsForNixFormatter (_,[]) = return ()
writeBracketsForNixFormatter (toString -> file, nixLines) = do
  liftIO $ putStrLn $ "Formattin file " ++ file ++ " partially"
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
      insertNum _ _ [] =
        error "Too short list to insert a line. The adding of the signal brackets is broken"
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

commit :: Sh ()
commit = do
    _ <- run "git" ["config", "user.name", "nixfmt"]
    _ <- run "git" ["config", "user.email", "nixfmt@serokell.io"]
    _ <- run "git" ["add", "."]
    _ <- run "git" ["commit", "-m", "Format code automatically"]
    _ <- run "git" ["push"]
    return ()
