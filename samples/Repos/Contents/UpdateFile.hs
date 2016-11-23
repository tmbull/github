{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
module Main where

import qualified GitHub.Auth as GitHub
import qualified GitHub.Endpoints.Repos.Contents as GitHub
import Data.ByteString.Base64 (decode, encode)
import Data.Proxy (Proxy(..))
import Data.String.Conversions (cs)
import Prelude hiding (truncate, getContents)
import System.Environment (getArgs)

main :: IO ()
main = do
  args <- getArgs
  result <- case args of
              [token, user, repo, path, branch, file] -> do
                fileContents <- readFile file
                let uploadContents = cs $ encode $ cs fileContents
                    auth = (GitHub.OAuth (cs token))
                    org = (GitHub.mkName (Proxy :: Proxy GitHub.Owner)(cs user))
                    repo' = (GitHub.mkName (Proxy :: Proxy GitHub.Repo)(cs repo))
                    branch' = (Just (cs branch))
                existingFile <- GitHub.contentsFor'
                  (Just auth)
                  org
                  repo'
                  (cs path)
                  branch'
                case existingFile of
                  Right (GitHub.ContentFile GitHub.ContentFileData{..}) -> do
                    let sha = GitHub.contentSha contentFileInfo
                    GitHub.updateFile'
                      auth
                      org
                      repo'
                      (cs path)
                      (GitHub.UpdateFile
                        "File created by createFile sample"
                        uploadContents
                        branch'
                        sha
                      )
                  Right (GitHub.ContentDirectory _) ->
                    error "Directories cannot be updated."
                  Left err -> error $ show err
                
              _ ->  error "usage: UpdateFile [token] <organization> <repo> \
                      \<file_path> <branch_name> <file_to_upload>"
  case result of
    Left err      -> putStrLn $ "Error: " ++ show err
    Right content -> print content
