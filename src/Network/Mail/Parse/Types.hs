module Network.Mail.Parse.Types where

import Data.Text
import GHC.Generics
import qualified Data.ByteString.Char8 as BS
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (ZonedTime(..), minutesToTimeZone, LocalTime(..),
  TimeOfDay(..))

type UID = Integer
type ErrorMessage = Text

data EmailMessage = EmailMessage {
  emailHeaders :: [Header],
  emailBodies :: ![EmailBody]
} deriving (Show, Eq, Generic)

type MessageId = Text
data Header = Date ZonedTime
            | From EmailAddress
            | ReplyTo EmailAddress
            | To [EmailAddress]
            | CC [EmailAddress]
            | BCC [EmailAddress]
            | MessageId MessageId
            | InReplyTo MessageId
            | References [MessageId]
            | Subject Text
            | Comments Text
            | Keywords [Text]
            | Header {
                headerName :: !Text,
                headerContents :: !Text
            }
            deriving (Eq, Show, Generic)

data EmailAddress = EmailAddress {
  emailAddress :: !Text,
  emailLabel :: Maybe Text
} deriving (Show, Generic)

instance Eq EmailAddress where
  x == y = emailAddress x == emailAddress y

-- |An email body contains the contents of an email part
-- up until the boundary marker.
data EmailBody
  -- |Body of a MIME message part. Contains headers
  = MessageBody EmailMessage
  -- = MIMEBody { mimeHeaders :: ![Header], mimeBody :: !Text}
  -- |A raw text body
  | TextBody !Text
  -- |Unknown body that may contain arbitrary binary
  | OtherBody !BS.ByteString
  -- |Attachment is part of a MIME message, but a rather special
  -- one. It's decoded from whatever the transfer encoding was applied
  -- and left as a raw sollection of bytes for your enjoyment
  | Attachment {
    attachmentHeaders :: ![Header],
    attachmentName :: !Text,
    attachmentBody :: Maybe BS.ByteString,
    -- |Location of the actual filename on disk
    storageFilename :: Maybe Text
  }
  deriving (Eq, Show, Generic)


instance Eq ZonedTime where
  x == y = (zonedTimeToLocalTime x) == (zonedTimeToLocalTime y) &&
           (zonedTimeZone x) == (zonedTimeZone y)

defaultZT :: ZonedTime
defaultZT = ZonedTime {
  zonedTimeZone = minutesToTimeZone 0,
  zonedTimeToLocalTime = LocalTime {
    localDay = ModifiedJulianDay {
      toModifiedJulianDay = 1
    },
    localTimeOfDay = TimeOfDay {
      todHour = 0,
      todMin = 0,
      todSec = 0
    }
  }
}
