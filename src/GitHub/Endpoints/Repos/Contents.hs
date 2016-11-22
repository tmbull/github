-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
-- The Github Repos API, as documented at
-- <http://developer.github.com/v3/repos/>
module GitHub.Endpoints.Repos.Contents (
    -- * Querying repository contents
    contentsFor,
    contentsFor',
    readmeFor,
    readmeFor',

    -- ** Create
    createFile',

    -- ** Edit

    -- ** Delete

    -- * Data
    module GitHub.Data,
    ) where

import GitHub.Data
import GitHub.Internal.Prelude
import GitHub.Request
import Prelude ()

import qualified Data.Text.Encoding as TE

-- | Create a file
-- See <https://developer.github.com/v3/repos/contents/#create-a-file>
createFile' :: Auth -> Name Owner -> Name Repo -> Text -> NewFile -> IO (Either Error CreatedFile)
createFile' auth user repo path newFile = executeRequest auth $ createFileR user repo path newFile

createFileR :: Name Owner -> Name Repo -> Text -> NewFile -> Request 'RW CreatedFile
createFileR user repo path newFile =
  command Put ["repos", toPathPart user, toPathPart repo, "contents", path] (encode newFile)

-- | The contents of a file or directory in a repo, given the repo owner, name, and path to the file
--
-- > contentsFor "thoughtbot" "paperclip" "README.md"
contentsFor :: Name Owner -> Name Repo -> Text -> Maybe Text -> IO (Either Error Content)
contentsFor = contentsFor' Nothing

-- | The contents of a file or directory in a repo, given the repo owner, name, and path to the file
-- With Authentication
--
-- > contentsFor' (Just (BasicAuth (user, password))) "thoughtbot" "paperclip" "README.md" Nothing
contentsFor' :: Maybe Auth ->  Name Owner -> Name Repo -> Text -> Maybe Text -> IO (Either Error Content)
contentsFor' auth user repo path ref =
    executeRequestMaybe auth $ contentsForR user repo path ref

contentsForR
    :: Name Owner
    -> Name Repo
    -> Text            -- ^ file or directory
    -> Maybe Text      -- ^ Git commit
    -> Request k Content
contentsForR user repo path ref =
    query ["repos", toPathPart user, toPathPart repo, "contents", path] qs
  where
    qs =  maybe [] (\r -> [("ref", Just . TE.encodeUtf8 $ r)]) ref

-- | The contents of a README file in a repo, given the repo owner and name
--
-- > readmeFor "thoughtbot" "paperclip"
readmeFor :: Name Owner -> Name Repo -> IO (Either Error Content)
readmeFor = readmeFor' Nothing

-- | The contents of a README file in a repo, given the repo owner and name
-- With Authentication
--
-- > readmeFor' (Just (BasicAuth (user, password))) "thoughtbot" "paperclip"
readmeFor' :: Maybe Auth -> Name Owner -> Name Repo -> IO (Either Error Content)
readmeFor' auth user repo =
    executeRequestMaybe auth $ readmeForR user repo

readmeForR :: Name Owner -> Name Repo -> Request k Content
readmeForR user repo =
    query ["repos", toPathPart user, toPathPart repo, "readme"] []

