module Main where

import System.Environment
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Applicative
import Control.Monad (mzero, forM, liftM, when)
import System.Process
import Data.Time.Clock
import qualified Github.Repos.Commits as Github
import Data.Time.Convenience
import Data.List (intercalate)

main = do
  args <- getArgs
  startingTime <- timeFor 1 Week Ago
  forM_ args $ \repoName -> do
    possibleCommits <- Github.commitsFor "thoughtbot" repoName
    case possibleCommits of
      (Left err) -> print err
      (Right allCommits) -> do
        let commits = commitsAfter startingTime allCommits
            groupedCommits = groupByAuthor commits
        when (not $ null groupedCommits) $ do
             putStrLn $ repoNameToHtml repoName
             putStrLn $ aggregatedGravatars $ map fst groupedCommits
        forM_ groupedCommits $ \(author,commits) ->  do
          putStrLn ""
          putStrLn $ userToHtml $ head commits -- can only get here if a commit exists for this author
          forM_ commits $ \commit -> do
            shortSha <- getShortSha $ Github.commitSha commit
            putStrLn $ commitToHtml commit shortSha

aggregatedGravatars :: [Github.GithubUser] -> String
aggregatedGravatars authors =
  intercalate "\n" $ map avatarizedHtml authors

avatarizedHtml user = userLink login avatarLink
  where login = Github.githubUserLogin user
        avatarUrl = Github.githubUserAvatarUrl user
        avatarLink = "<img src=\"" ++ avatarUrl ++ "&s=140\" alt=\"" ++
          login ++ "'s avatar\">"

userToHtml :: Github.Commit -> String
userToHtml commit =
  "  " ++ (Github.gitUserName gitAuthor) ++
    " (" ++ (userLink login login) ++ ")"
  where
    (Just githubAuthor) = Github.commitAuthor commit
    gitAuthor = Github.gitCommitAuthor $ Github.commitGitCommit commit
    login = Github.githubUserLogin githubAuthor

userLink login content =
  "<a href=\"http://github.com/" ++
    login ++ "\" title=\"" ++ login ++ "\">" ++ content ++ "</a>"

commitToHtml :: Github.Commit -> String -> String
commitToHtml commit shortSha =
  "    <a href=\"" ++ commitUrl ++ "\" title=\"" ++ (htmlEscape $ Github.gitCommitMessage $ Github.commitGitCommit commit) ++ "\">" ++ shortSha ++ "</a>"
  where
  htmlEscape "" = ""
  htmlEscape (s:ss)
    | s == '"' = "&quot;" ++ htmlEscape ss
    | otherwise = s : htmlEscape ss
  commitUrl =
    "https://github.com/thoughtbot/paperclip/commit/" ++ (Github.commitSha commit)

repoNameToHtml :: String -> String
repoNameToHtml repoName =
  "<h2>" ++ repoName ++ "</h2>"

groupByAuthor :: [Github.Commit] -> [(Github.GithubUser, [Github.Commit])]
groupByAuthor commits = Map.assocs $ groupByAuthor' commits Map.empty
  where
  groupByAuthor' [] hash = hash
  groupByAuthor' (x:xs) hash =
    groupByAuthor' xs $ inserter hash
    where
      inserter =
        case Github.commitAuthor x of
          (Just author) -> Map.insertWith (++) author [x]
          Nothing       -> id

commitsAfter :: UTCTime -> [Github.Commit] -> [Github.Commit]
commitsAfter startingTime commits =
  filter (\commit -> (commitDate commit) > startingTime)
         commits
  where
  commitDate commit =
    Github.fromGithubDate $ Github.gitUserDate $ Github.gitCommitCommitter $ Github.commitGitCommit commit

getShortSha :: String -> IO String
getShortSha sha =
  init `liftM` readProcess "git" ["rev-parse", "--short", sha] ""
