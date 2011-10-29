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
import Control.Monad (mzero, forM, liftM)

import qualified Data.Text as T

import Data.Typeable
import Data.Data

import System.Process

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
  } deriving (Show, Typeable, Data)

data Commits = Commits {
  commitsCommits :: [Commit]
  } deriving (Show, Typeable, Data)

main = do
  args <- getArgs
  forM_ args $ \repoName -> do
    commits <- getCommits repoName
    putStrLn $ repoNameToHtml repoName
    let groupedCommits = groupByAuthor $ commitsForRepo repoName commits
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

getCommits :: String -> IO [Commit]
getCommits repoName = do
  jsonString <- openUrl $ githubUrlFor repoName
  let parsed = parse (fromJSON <$> json) jsonString
  case parsed of
       Data.Attoparsec.Done _ jsonResult -> do
         case jsonResult of
              (Success s) -> handleCommits $ commitsCommits s
       x -> do
         print x
         return []
  where
    handleCommits commits =
      forM commits $ \commit -> do
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
    }

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
  parseJSON _          = mzero

instance FromJSON Commits where
  parseJSON (Object o) = Commits <$> o .: "commits"
  parseJSON _          = mzero

githubUrlFor :: String -> String
githubUrlFor repoName =
  "http://github.com/api/v2/json/commits/list/thoughtbot/" ++ repoName ++ "/master"

openUrl :: String -> IO BS.ByteString
openUrl url =
  getResponseBody =<< simpleHTTP (mkRequest GET (fromJust $ parseURI url))

--

getShortSha :: String -> IO String
getShortSha sha =
  init `liftM` readProcess "git" ["rev-parse", "--short", sha] ""
