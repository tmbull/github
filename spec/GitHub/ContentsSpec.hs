{-# LANGUAGE OverloadedStrings #-}
module GitHub.ContentsSpec where

import qualified GitHub

import GitHub.Auth                     (Auth (..))
import GitHub.Data.Content             (Content(..))
import GitHub.Endpoints.Repos.Contents (contentsFor', readmeFor')

import Data.ByteString.Char8 (lines, unlines)
import Data.ByteString.Base64 (decode)
import Data.Either.Compat (isRight)
import Data.Proxy         (Proxy(..))
import Data.String        (fromString)
import Data.String.Conversions (cs)
import Data.Text          (Text, isInfixOf)
import Data.Text.Encoding (decodeUtf8)
import Prelude hiding (lines, unlines)
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

-- lines/unlines is necessary because GitHub includes newline characters in
-- their base64 output which is not supported by Data.ByteString.Base64
decodeText :: Text -> Text
decodeText = cs . unlines . map (fromRightS . decode) . lines . cs

spec :: Spec
spec = do
  describe "contentsFor'" $ do
    it "works" $ withAuth $ \ auth -> do
      let org = GitHub.mkName (Proxy :: Proxy GitHub.Owner) "phadej"
          repo = GitHub.mkName (Proxy :: Proxy GitHub.Repo) "github"
      content <- contentsFor'
                   (Just auth)
                   org
                   repo
                   "README.md"
                   (Just "b0e558cb1c504b8f4488e1673b3cf8c5b0334a0b")
      content `shouldSatisfy` isRight
      let content' = decodeText $ fileContentFromContentFile $ fromRightS content
      cs content' `shouldSatisfy` ("The Github API v3 for Haskell." `isInfixOf`)
  describe "readmeFor'" $ do
    it "works" $ withAuth $ \ auth -> do
      let org = GitHub.mkName (Proxy :: Proxy GitHub.Owner) "phadej"
          repo = GitHub.mkName (Proxy :: Proxy GitHub.Repo) "github"
      readme <- readmeFor'
                   (Just auth)
                   org
                   repo
      readme `shouldSatisfy` isRight
      let readme' = decodeText $ fileContentFromContentFile $ fromRightS readme
      cs readme' `shouldSatisfy` ("The Github API v3 for Haskell." `isInfixOf`)
