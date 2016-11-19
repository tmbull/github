{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import Data.String
import System.Environment
import qualified GitHub
import qualified GitHub.Endpoints.Organizations.Teams as GitHub

main :: IO ()
main = do
    args <- getArgs
    possibleRepos <- case args of
      [team_id, token] -> GitHub.listTeamRepos' (Just $ GitHub.OAuth $ fromString token) (GitHub.mkTeamId $ read team_id)
      [team_id]        -> GitHub.listTeamRepos (GitHub.mkTeamId $ read team_id)
      _                -> error "usage: TeamListRepos <team_id> [auth token]"
    case possibleRepos of
      Left err    -> putStrLn $ "Error: " ++ show err
      Right repos -> putStrLn $ show repos
