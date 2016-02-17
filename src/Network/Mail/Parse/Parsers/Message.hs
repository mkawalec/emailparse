module Network.Mail.Parse.Parsers.Message (messageParser) where

import Data.Attoparsec.ByteString

import Data.List (find)
import Data.Maybe
import Data.Either (isRight)
import Data.Either.Unwrap (fromRight)
import Control.Monad (liftM)

import Network.Mail.Parse.Types
import Network.Mail.Parse.Utils
import Data.Text (Text)

import Network.Mail.Parse.Parsers.MIME (parseMIME)
import Network.Mail.Parse.Parsers.Utils (isMIME)
import Network.Mail.Parse.Decoders.BodyDecoder (decodeTextBody)
import Network.Mail.Parse.Parsers.Header (headerParser)
import Network.Mail.Parse.Parsers.HeaderFields

parseHeader :: [Header] ->
               (Text -> Either ErrorMessage a) ->
               Text ->
               Either ErrorMessage a
parseHeader headers parser headerN = liftM headerContents header >>= parser
  where header = findHeader headerN headers

-- |Parses a single message
messageParser :: Parser (Either ErrorMessage EmailMessage)
messageParser = do
  headers <- manyTill' headerParser $ string "\r\n"
  body <- takeByteString

  -- Parse MIME if the message is in a MIME format
  let parsedBody = if isJust $ find isMIME headers
      then parseMIME headers body
      else Right [TextBody $ decodeTextBody headers body]

  let p = parseHeader headers
  let e2m = eitherToMaybe
  let date = p parseTime "date"
  let fromH = p parseEmailAddress "from"

  if isRight date && isRight fromH
    then return $! parsedBody >>= \b -> return EmailMessage {
        flags = Nothing
      , origDate = fromRight date
      , from = fromRight fromH
      , sender = e2m $ p parseEmailAddress "from"
      , replyTo = e2m $ p parseEmailAddress "reply-to"
      , to = e2m $ p parseEmailAddressList "to"
      , cc = e2m $ p parseEmailAddressList "cc"
      , bcc = e2m $ p parseEmailAddressList "bcc"
      , messageId = e2m $ p Right "message-id"
      , inReplyTo = e2m $ p Right "in-reply-to"
      , references = e2m $ p (parseTextList " ") "references"
      , subject = e2m $ p Right "subject"
      , comments = e2m $ p Right "comments"
      , keywords = e2m $ p (parseTextList ",") "keywords"
      , emailHeaders=headers
      , emailBodies=b
      }
    else return $ Left "Headers parse error"
