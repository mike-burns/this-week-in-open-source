{-# LANGUAGE DeriveDataTypeable, OverloadedStrings #-}

module Main where

import System.Environment
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

import Network.HTTP
import Network.URI

import Data.Aeson
import Data.Attoparsec
import qualified Data.ByteString as BS
import Data.Maybe

import Control.Applicative
import Control.Monad (mzero, forM, liftM, when)

import qualified Data.Text as T

import Data.Typeable
import Data.Data

import System.Process

import Data.Time.Clock
import System.Locale
import Data.Time.Format

import Network.Curl.Download

newtype GithubTime = GithubTime { fromGithubTime :: UTCTime }
  deriving (Show, Data, Typeable)

data Author = Author {
   authorName :: String
  ,authorLogin :: String
  } deriving (Show, Ord, Eq, Typeable, Data)

data Commit = Commit {
   commitRepoName :: String
  ,commitUrl :: String
  ,commitMessage :: String
  ,commitShortSha  :: String
  ,commitAuthor :: Author
  ,commitDate :: GithubTime
  } deriving (Show, Typeable, Data)

data Commits = Commits {
  commitsCommits :: [Commit]
  } deriving (Show, Typeable, Data)

main = do
  args <- getArgs
  startingTime <- oneWeekAgo
  forM_ args $ \repoName -> do
    commits <- getCommits startingTime repoName
    let groupedCommits = groupByAuthor $ commitsForRepo repoName commits
    when (not $ null groupedCommits) (putStrLn $ repoNameToHtml repoName)
    forM_ groupedCommits $ \(author,commits) ->  do
      putStrLn ""
      putStrLn $ authorToHtml author
      forM_ commits $ putStrLn . commitToHtml

authorToHtml :: Author -> String
authorToHtml author =
  "  " ++ (authorName author) ++ " (<a href=\"http://github.com/" ++
    (authorLogin author) ++ "\">" ++ (authorLogin author) ++ "</a>)"

commitToHtml :: Commit -> String
commitToHtml commit =
  "    <a href=\"" ++ (commitUrl commit) ++ "\" title=\"" ++ (htmlEscape $ commitMessage commit) ++ "\">" ++ (commitShortSha commit) ++ "</a>"
  where
  htmlEscape "" = ""
  htmlEscape (s:ss)
    | s == '"' = "&quot;" ++ htmlEscape ss
    | otherwise = s : htmlEscape ss

repoNameToHtml :: String -> String
repoNameToHtml repoName =
  "<h2>" ++ repoName ++ "</h2>"

groupByAuthor :: [Commit] -> [(Author, [Commit])]
groupByAuthor commits = Map.assocs $ groupByAuthor' commits Map.empty
  where
  groupByAuthor' [] hash = hash
  groupByAuthor' (x:xs) hash =
    groupByAuthor' xs $ Map.insertWith (++) (commitAuthor x) [x] hash

commitsForRepo :: String -> [Commit] -> [Commit]
commitsForRepo repoName commits =
  filter (\commit -> repoName == commitRepoName commit) commits

--

getCommits :: UTCTime -> String -> IO [Commit]
getCommits startingTime repoName = do
  (Right jsonString) <- openURI $ githubUrlFor repoName
  let parsed = parse (fromJSON <$> json) jsonString
  case parsed of
       Data.Attoparsec.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> handleCommits $ commitsCommits s
              x -> print x >> return []
       x -> do
         print x
         return []
  where
    handleCommits commits =
      forM (commitsAfter startingTime commits) $ \commit -> do
        shortSha <- getShortSha $ commitShortSha commit
        return $ processCommitFor repoName shortSha commit

processCommitFor :: String -> String -> Commit -> Commit
processCommitFor repoName shortSha commit =
  Commit {
     commitRepoName = repoName
    ,commitUrl = "http://github.com" ++ commitUrl commit
    ,commitMessage = commitMessage commit
    ,commitShortSha = shortSha
    ,commitAuthor = commitAuthor commit
    ,commitDate = commitDate commit
    }

commitsAfter :: UTCTime -> [Commit] -> [Commit]
commitsAfter startingTime commits =
  filter (\commit -> fromGithubTime (commitDate commit) > startingTime)
         commits

oneWeekAgo :: IO UTCTime
oneWeekAgo = addUTCTime oneWeek <$> getCurrentTime
oneWeek = (-7 * 60 * 60 * 24)

instance FromJSON Author where
  parseJSON (Object o) = Author <$> o .: "name" <*> o .: "login"
  parseJSON _          = mzero

instance FromJSON Commit where
  parseJSON (Object o) =
    Commit "-"
           <$> o .: "url"
           <*> o .: "message"
           <*> o .: "id"
           <*> o .: "author"
           <*> o .: "committed_date"
  parseJSON _          = mzero

instance FromJSON Commits where
  parseJSON (Object o) = Commits <$> o .: "commits"
  parseJSON _          = mzero

instance FromJSON GithubTime where
  parseJSON (String t) =
    case parseTime defaultTimeLocale "%FT%T%Z" (T.unpack t) of
         Just d -> pure $ GithubTime d
         _      -> fail "could not parse Github datetime"
  parseJSON v   = mzero

githubUrlFor :: String -> String
githubUrlFor repoName =
  "http://github.com/api/v2/json/commits/list/thoughtbot/" ++ repoName ++ "/master"

--

getShortSha :: String -> IO String
getShortSha sha =
  init `liftM` readProcess "git" ["rev-parse", "--short", sha] ""
