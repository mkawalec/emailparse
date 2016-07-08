module Network.Mail.Parse.Parsers.Message (messageParser) where

import Data.Attoparsec.ByteString

import Data.List (find)
import Data.Maybe
import Data.Either (isRight)
import Control.Monad (liftM)

import Network.Mail.Parse.Types
import Network.Mail.Parse.Utils

import Network.Mail.Parse.Parsers.Utils (isMIME, discoverAttachment)
import Network.Mail.Parse.Parsers.Multipart (parseMultipart)
import Network.Mail.Parse.Decoders.BodyDecoder (decodeBody, decodeTextBody)
import Network.Mail.Parse.Parsers.Header (headerParser)
import Network.Mail.Parse.Parsers.HeaderFields
import qualified Data.Text as T

import qualified Data.ByteString.Char8 as BSC
import Data.Either.Utils (maybeToEither)

import Data.Text.Encoding (encodeUtf8)
import Data.Either.Combinators (fromRight', fromRight, mapLeft)

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type

import Control.Monad (join)

parseHeader :: Header -> Header
parseHeader header = fromRight header parsedHeader
  where hname        = headerName header
        contents     = headerContents header
        parsedHeader = case T.toLower hname of
          "date" -> liftM Date $ parseTime contents
          "from" -> liftM From $ parseEmailAddress contents
          "reply-to" -> liftM ReplyTo $ parseEmailAddress contents
          "to" -> liftM To $ parseEmailAddressList contents
          "cc" -> liftM CC $ parseEmailAddressList contents
          "bcc" -> liftM BCC $ parseEmailAddressList contents
          "message-id" -> MessageId <$> parseMessageId contents
          "in-reply-to" -> Right $ InReplyTo contents
          "references" -> liftM References $ parseTextList " " contents
          "subject" -> Right $ Subject contents
          "comments" -> Right $ Comments contents
          "keywords" -> liftM Keywords $ parseTextList "," contents
          _ -> Right header

-- |Parses a single message
messageParser :: Maybe [Header] -> -- ^ Headers, if they were already parsed
                Maybe [Header] -> -- ^ Context headers, useful is encoding is only
                                  -- defined in the message above, for instance
                Parser (Either ErrorMessage EmailMessage)
messageParser headersIn helperHeadersIn = do
  headers <- if isJust headersIn
    then return . fromJust $ headersIn
    else manyTill' headerParser $ string "\r\n"
  let helperHeaders = if isJust helperHeadersIn then fromJust helperHeadersIn else []

  body <- takeByteString
  let parsedHeaders = map parseHeader headers

  -- Parse MIME if the message is in a MIME format
  let parsedBody = if isJust $ find isMIME headers
      then parseMIME (headers ++ helperHeaders) body
      else Right [TextBody $ decodeTextBody (headers ++ helperHeaders) body]

  return $! parsedBody >>= return . EmailMessage parsedHeaders


-- |Parses a MIME message part. Needs headers from the actual message
-- in case the MIME block misses some encoding blocks
mimeParser :: [Header] -> Parser (Either ErrorMessage EmailBody)
mimeParser bodyHeaders = do
  headers <- manyTill' headerParser $ string "\r\n"
  let isAttachment = discoverAttachment headers
  if isJust isAttachment
    then do
      body <- takeByteString
      let filename = fromJust isAttachment
      let decodedBody = decodeBody headers body
      return . Right $ Attachment headers filename (Just decodedBody) Nothing
    else (liftM . liftM) MessageBody $ messageParser (Just headers) (Just bodyHeaders)

-- |Parse a set of parts.
multipartParser :: [Header] -> [BSC.ByteString] -> Either ErrorMessage [EmailBody]
multipartParser bodyHeaders parts = do
  mapM (\p -> join $ mapLeft T.pack $ parseOnly (mimeParser bodyHeaders) p) parts

-- |Parse a mime encoded body.
parseMIME :: [Header] -> BSC.ByteString -> Either ErrorMessage [EmailBody]
parseMIME headers body = if isRight msgType then
  (case mimeType . fromRight' $ msgType of
    Multipart _ -> multiParsed >>= multipartParser headers
    Text _ -> Right decodedBody
    _ -> Left "mimetype not supported")
  else Right decodedBody
  where msgType = findHeader "Content-Type" headers >>=
          Right . parseMIMEType . headerContents >>=
          maybeToEither "Couldn't parse message type"
        multiParsed = msgType >>=
          \x -> maybeToEither "" $ find (\p -> paramName p == "boundary") (mimeParams x) >>=
          return . encodeUtf8 . paramValue >>=
          \b -> eitherToMaybe $ parseOnly (parseMultipart b) body
        decodedBody = [TextBody $ decodeTextBody headers body]
