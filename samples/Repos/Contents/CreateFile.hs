{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified GitHub.Auth as GitHub
import qualified GitHub.Endpoints.Repos.Contents as GitHub
import Data.ByteString.Base64 (encode)
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
                GitHub.createFile'
                  (GitHub.OAuth (cs token))
                  (GitHub.mkName (Proxy :: Proxy GitHub.Owner)(cs user))
                  (GitHub.mkName (Proxy :: Proxy GitHub.Repo)(cs repo))
                  (cs path)
                  (GitHub.NewFile
                    "File created by createFile sample"
                    uploadContents
                    (Just (cs branch))
                  )
              _                                       ->
                error "usage: CreateFile [token] <organization> <repo> \
                      \<file_path> <branch_name> <file_to_upload>"
  case result of
    Left err      -> putStrLn $ "Error: " ++ show err
    Right content -> print content
