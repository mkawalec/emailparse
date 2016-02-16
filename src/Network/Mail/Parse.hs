module Network.Mail.Parse (parseMessage) where

import Types
import Network.Mail.Parse.Types
import Network.Mail.Parse.Parsers.Message (messageParser)

import qualified Data.ByteString.Char8 as BSC
import Data.Attoparsec.ByteString
import Control.Monad (join)

-- |Parses a single message of any mimetype
parseMessage :: BSC.ByteString -> IO (Either ErrorMessage EmailMessage)
parseMessage message =
  return . join $ parseOnly messageParser message
