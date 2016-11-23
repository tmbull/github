-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Content where

import GitHub.Data.GitData (GitCommit(..))
import GitHub.Data.URL
import GitHub.Internal.Prelude
import Prelude ()

data Content
  = ContentFile !ContentFileData
  | ContentDirectory !(Vector ContentItem)
 deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData Content where rnf = genericRnf
instance Binary Content

data ContentFileData = ContentFileData {
   contentFileInfo     :: !ContentInfo
  ,contentFileEncoding :: !Text
  ,contentFileSize     :: !Int
  ,contentFileContent  :: !Text
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentFileData where rnf = genericRnf
instance Binary ContentFileData

-- | An item in a directory listing.
data ContentItem = ContentItem {
   contentItemType :: !ContentItemType
  ,contentItemInfo :: !ContentInfo
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItem where rnf = genericRnf
instance Binary ContentItem

data ContentItemType = ItemFile | ItemDir
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentItemType where rnf = genericRnf
instance Binary ContentItemType

-- | Information common to both kinds of Content: files and directories.
data ContentInfo = ContentInfo {
   contentName    :: !Text
  ,contentPath    :: !Text
  ,contentSha     :: !Text
  ,contentUrl     :: !URL
  ,contentGitUrl  :: !URL
  ,contentHtmlUrl :: !URL
} deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData ContentInfo where rnf = genericRnf
instance Binary ContentInfo

-- | A new file to be committed to the repo.
data NewFile = NewFile {
    newFileMessage :: !Text
  , newFileContent :: !Text
  , newFileBranch :: !(Maybe Text)
  } deriving (Show, Data, Typeable, Eq, Ord, Generic)

-- | A file to be updated on the repo.
data UpdateFile = UpdateFile {
    updateFileMessage :: !Text
  , updateFileContent :: !Text
  , updateFileBranch :: !(Maybe Text)
  , updateFileSha :: !Text
  } deriving (Show, Data, Typeable, Eq, Ord, Generic)


-- | A file that has been successfully created on a repo.
data CreatedFile = CreatedFile {
    createdFileContent :: !ContentInfo
  , createdFileCommit :: !GitCommit
  } deriving (Show, Data, Typeable, Eq, Ord, Generic)

data Author = Author {
    authorName :: !Text
  , authorEmail :: !Text
  }

instance FromJSON Content where
  parseJSON o@(Object _) = ContentFile <$> parseJSON o
  parseJSON (Array os) = ContentDirectory <$> traverse parseJSON os
  parseJSON _ = fail "Could not build a Content"

instance FromJSON ContentFileData where
  parseJSON = withObject "ContentFileData" $ \o ->
    ContentFileData <$> parseJSON (Object o)
                    <*> o .: "encoding"
                    <*> o .: "size"
                    <*> o .: "content"

instance FromJSON ContentItem where
  parseJSON = withObject "ContentItem" $ \o ->
    ContentItem <$> o .: "type"
                <*> parseJSON (Object o)

instance FromJSON ContentItemType where
  parseJSON = withText "ContentItemType" $ \t ->
      case t of
          "file" -> return ItemFile
          "dir"  -> return ItemDir
          _      -> fail $ "Invalid ContentItemType: " ++ unpack t

instance FromJSON ContentInfo where
  parseJSON = withObject "ContentInfo" $ \o ->
    ContentInfo <$> o .: "name"
                <*> o .: "path"
                <*> o .: "sha"
                <*> o .: "url"
                <*> o .: "git_url"
                <*> o .: "html_url"

instance ToJSON NewFile where
    toJSON (NewFile msg content branch) = object
        [ "message" .= msg
        , "content" .= content
        , "branch" .= branch
        ]

instance FromJSON NewFile where
  parseJSON = withObject "NewFile" $ \o ->
    NewFile <$> o .: "message"
            <*> o .: "content"
            <*> o .: "branch"

instance ToJSON UpdateFile where
    toJSON (UpdateFile msg content branch sha) = object
        [ "message" .= msg
        , "content" .= content
        , "branch" .= branch
        , "sha" .= sha
        ]

instance FromJSON CreatedFile where
  parseJSON = withObject "CreatedFile" $ \o ->
    CreatedFile <$> o .: "content"
                <*> o .: "commit"
