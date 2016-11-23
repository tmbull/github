{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified GitHub.Auth as GitHub
import qualified GitHub.Endpoints.Repos.Contents as GitHub
import Data.ByteString.Char8 (lines, unlines, filter, readFile)
import Data.ByteString.Base64 (decode, encode)
import Data.Proxy (Proxy(..))
import Data.String.Conversions (cs)
import Data.Text (Text)
import Prelude hiding (truncate, getContents, lines, unlines, filter, readFile)
import System.Environment (getArgs)
import Text.Regex

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [token, user, repo, branch] -> do
                let auth = (GitHub.OAuth (cs token))
                    org = (GitHub.mkName (Proxy :: Proxy GitHub.Owner)(cs user))
                    repo' = (GitHub.mkName (Proxy :: Proxy GitHub.Repo)(cs repo))
                    branch' = (Just (cs branch))
                existingFile <- GitHub.contentsFor'
                  (Just auth)
                  org
                  repo'
                  "rakefile"
                  branch'
                case existingFile of
                  Right (GitHub.ContentFile GitHub.ContentFileData{..}) -> do
                    let sha = GitHub.contentSha contentFileInfo
                        rakefile = cs $ decodeText contentFileContent
                        regex = mkRegexWithOpts "(^version[ ]*=[ ]*')([0-9]+).([0-9]+)(.'.*)" True False
                        newRakefile = replaceVersionInRakefile regex rakefile
                    putStr rakefile
                    putStr newRakefile
                    jenkinsfile <- readFile "Jenkinsfile"
                    GitHub.updateFile'
                      auth
                      org
                      repo'
                      "rakefile"
                      (GitHub.UpdateFile
                        "Updating rakefile for pipeline build."
                        (cs $ encode $ cs newRakefile)
                        branch'
                        sha
                      )
                    GitHub.createFile'
                      auth
                      org
                      repo'
                      "Jenkinsfile"
                      (GitHub.NewFile
                        "Adding Jenkinsfile to trigger pipeline build."
                        (cs $ encode jenkinsfile)
                        branch'
                      )
                  Right (GitHub.ContentDirectory _) ->
                    error "Directories cannot be updated."
                  Left err -> error $ show err
              _ ->  error "usage: UpdateFile [token] <organization> <repo> \
                      \<branch_name>"
  case result of
    Left err      -> putStrLn $ "Error: " ++ show err
    Right content -> print content

replaceVersionInRakefile :: Regex -> String -> String
replaceVersionInRakefile regex rakefile =
  let replacementStr = case matchRegex regex rakefile of
                         Just [prefix,major,minor,suffix] ->
                           prefix ++ major ++ "." ++ show (read minor + 1) ++ suffix
                         Just other ->
                           error "Version regex did not match expected length: " ++ show other
                         Nothing ->
                           error "Failed to find version regex"
  in
    subRegex regex rakefile replacementStr
-- lines/unlines is necessary because GitHub includes newline characters in
-- their base64 output which is not supported by Data.ByteString.Base64
decodeText :: Text -> Text
decodeText = cs . (fromRightS . decode) . filter ( /= '\n') . cs

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

data Version = Version {
    major :: Int
  , minor :: Int
  }

instance Show Version where
  show (Version major minor) = show major ++ "." ++ show minor
