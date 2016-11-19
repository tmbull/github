module Main where

import Data.Proxy
import Data.Text
import System.Environment
import qualified GitHub
import qualified GitHub.Endpoints.Repos as Github

main :: IO ()
main = do
  args <- getArgs
  print args
  possibleReadme <- case args of
    [orgName, repoName] -> Github.readmeFor
                           (GitHub.mkName (Proxy :: Proxy GitHub.Owner)(pack orgName))
                           (GitHub.mkName (Proxy :: Proxy GitHub.Repo) (pack repoName))
    _ -> error "Usage: GetReadme <org_name> <repo_name>"
  case possibleReadme of
    (Left err) -> putStrLn $ "Error: " ++ show err
    (Right (Github.ContentFile cd)) -> print cd
    (Right c) -> putStrLn $ "This should never happen. Returned: " ++ show c
