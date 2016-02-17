module Network.Mail.Parse.Types where

import Data.Text
import qualified Data.ByteString.Char8 as BS
import Data.Time.Calendar (Day(..))
import Data.Time.LocalTime (ZonedTime(..), minutesToTimeZone, LocalTime(..),
  TimeOfDay(..))

type UID = Integer
type ErrorMessage = Text

data EmailMessage = EmailMessage {
  origDate :: Maybe ZonedTime,
  from :: Maybe EmailAddress,
  sender :: Maybe EmailAddress,
  replyTo :: Maybe EmailAddress,
  to :: Maybe [EmailAddress],
  cc :: Maybe [EmailAddress],
  bcc :: Maybe [EmailAddress],
  messageId :: Maybe MessageId,
  inReplyTo :: Maybe MessageId,
  references :: Maybe [MessageId],
  subject :: Maybe Text,
  comments :: Maybe Text,
  keywords :: Maybe [Text],

  emailHeaders :: ![Header],
  emailBodies :: ![EmailBody]
} deriving (Show, Eq)

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

type MessageId = Text

data EmailAddress = EmailAddress {
  emailAddress :: !Text,
  emailLabel :: Maybe Text
} deriving (Show)

instance Eq EmailAddress where
  x == y = emailAddress x == emailAddress y

data Header = Header {
  headerName :: !Text,
  headerContents :: !Text
} deriving (Eq, Show)

-- |An email body contains the contents of an email part
-- up until the boundary marker.
data EmailBody
  -- |Body of a MIME message part. Contains headers
  = MIMEBody EmailMessage
  -- = MIMEBody { mimeHeaders :: ![Header], mimeBody :: !Text}
  -- |If the message contained no MIME information, it's probably
  -- just some text. Best guess decoding into UTF-8 is applied
  | TextBody !Text
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
  deriving (Eq, Show)
