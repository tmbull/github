{-# LANGUAGE OverloadedStrings #-}
module GitHub.ContentsSpec where

import qualified GitHub

import GitHub.Auth                     (Auth (..))
import GitHub.Data.Content             (Content(..))
import GitHub.Endpoints.Repos.Contents (contentsFor')

import Data.Either.Compat (isRight)
import Data.Proxy         (Proxy(..))
import Data.String        (fromString)
import Data.Text          (Text, isInfixOf)
import System.Environment (lookupEnv)
import Test.Hspec         (Spec, describe, it, pendingWith, shouldSatisfy)

fromRightS :: Show a => Either a b -> b
fromRightS (Right b) = b
fromRightS (Left a) = error $ "Expected a Right and got a Left" ++ show a

withAuth :: (Auth -> IO ()) -> IO ()
withAuth action = do
  mtoken <- lookupEnv "GITHUB_TOKEN"
  case mtoken of
    Nothing    -> pendingWith "no GITHUB_TOKEN"
    Just token -> action (OAuth $ fromString token)

isContentFile :: Content -> Bool
isContentFile (ContentFile _) = True
isContentFile _ = False

fileContentFromContentFile :: Content -> Text
fileContentFromContentFile (ContentFile f) = GitHub.contentFileContent f
fileContentFromContentFile (ContentDirectory d) = error $ "Expected Content file, but was " ++ show d

spec :: Spec
spec = do
  describe "contentsForR" $ do
    it "works" $ withAuth $ \ auth -> do
      let org = GitHub.mkName (Proxy :: Proxy GitHub.Owner) "phadej"
          repo = GitHub.mkName (Proxy :: Proxy GitHub.Repo) "github"
      content <- contentsFor' (Just auth) org repo "README.md" (Just "b0e558cb1c504b8f4488e1673b3cf8c5b0334a0b")
      content `shouldSatisfy` isRight
      let content' = fromRightS content
      content'  `shouldSatisfy` isContentFile
      fileContentFromContentFile content' `shouldSatisfy` ("The Github API v3 for Haskell." `isInfixOf`)
