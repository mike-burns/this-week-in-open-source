module Main where

import System.Environment
import Data.Foldable
import Data.Map (Map)
import qualified Data.Map as Map

data Author = Author {
   authorName :: String
  ,authorLogin :: String
  } deriving (Ord, Eq)

data Commit = Commit {
   commitRepoName :: String
  ,commitUrl :: String
  ,commitMessage :: String
  ,commitShortSha  :: String
  ,commitAuthor :: Author
  }

main = do
  args <- getArgs
  forM_ args $ \repoName -> do
    commits <- getCommits repoName
    putStrLn $ repoNameToHtml repoName
    let groupedCommits = groupByAuthor $ commitsForRepo repoName commits
    forM_ groupedCommits $ \(author,commits) ->  do
      putStrLn $ authorToHtml author
      forM_ commits $ putStrLn . commitToHtml

authorToHtml :: Author -> String
authorToHtml author =
  (authorName author) ++ " (<a href=\"http://github.com/" ++
    (authorLogin author) ++ "\">" ++ (authorLogin author) ++ "</a>"

commitToHtml :: Commit -> String
commitToHtml commit =
  "<a href=\"" ++ (commitUrl commit) ++ "\" title=\"" ++ (htmlEscape $ commitMessage commit) ++ "\">" ++ (commitShortSha commit) ++ "</a>"
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

getCommits :: String -> IO [Commit]
getCommits _ =
  return $ [Commit {
     commitRepoName = "shoulda"
    ,commitUrl = "http://github.com/thoughtbot/shoulda/blah" -- prepend with "http://github.com"
    ,commitMessage = "Re-wrote as RSpec"
    ,commitShortSha = "abc123" -- actually needs to use git
    ,commitAuthor = Author { authorName = "Mike Burns", authorLogin = "mike-burns" }
    }]
