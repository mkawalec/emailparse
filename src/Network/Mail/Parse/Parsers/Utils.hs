module Network.Mail.Parse.Parsers.Utils where

import Network.Mail.Parse.Types
import Network.Mail.Parse.Utils

import qualified Data.Text as T
import Data.List
import Control.Monad (liftM)


-- |Check if the given headers represent an attachment
discoverAttachment :: [Header] -> Maybe T.Text
discoverAttachment headers = hdr >>= findAttachmentName . headerContents
  where hdr = find (\x -> (T.toLower . headerName $ x) == "content-disposition") headers

-- |When provided with a content-disposition header,
-- checks if it represents an attachment. If it does it returns
-- it's name, otherwise Nothing
findAttachmentName :: T.Text -> Maybe T.Text
findAttachmentName header =
  if dispType == "attachment" || dispType == "inline"
    then if length split == 1
      then Just ""
      else liftM (T.strip . T.dropAround (== '"') . (!! 1)) filenameParam
    else Nothing
  where split = T.splitOn ";" header
        dispType = T.toLower . T.strip . head $ split
        paramSplit = map (T.splitOn "=") (tail split)
        filenameParam = find (\x -> T.strip (head x) == "filename") paramSplit

-- |Decide if the header contains a valid MIME info
isMIME :: Header -> Bool
isMIME header = isNameValid && isVersionValid
  where isNameValid    = T.toLower (headerName header) == "mime-version"
        version        = commentRemover $ headerContents header
        isVersionValid = version == "1.0"
