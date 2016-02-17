module Network.Mail.Parse.Parsers.Message (messageParser) where

import Data.Attoparsec.ByteString

import Data.List (find)
import Data.Maybe
import Data.Either (isRight, isLeft)
import Data.Either.Unwrap (fromRight)
import Control.Monad (liftM)

import Network.Mail.Parse.Types
import Network.Mail.Parse.Utils
import Data.Text (Text)

import Network.Mail.Parse.Parsers.Utils (isMIME, discoverAttachment)
import Network.Mail.Parse.Parsers.Multipart (parseMultipart)
import Network.Mail.Parse.Decoders.BodyDecoder (decodeBody, decodeTextBody)
import Network.Mail.Parse.Parsers.Header (headerParser)
import Network.Mail.Parse.Parsers.HeaderFields

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Either.Utils (maybeToEither)
import Data.Either.Combinators (mapLeft)
import Data.Attoparsec.ByteString
import Data.List (find)
import Data.Maybe (fromJust, isJust)

import Data.Text.Encoding (encodeUtf8)
import Data.Either (rights, isRight)
import Data.Either.Combinators (fromRight')

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type

import Control.Monad (foldM, mapM, join)
import qualified Debug.Trace as DT

parseHeader :: [Header] ->
  (Text -> Either ErrorMessage a) -> Text ->  Either ErrorMessage a
parseHeader headers parser headerN = liftM headerContents header >>= parser
  where header = findHeader headerN headers

-- |Parses a single message
messageParser :: Maybe [Header] -> Maybe [Header] -> Parser (Either ErrorMessage EmailMessage)
messageParser headersIn helperHeadersIn = DT.trace "started" $ do
  let headers = []
  headers <- if isJust headersIn
    then return . fromJust $ headersIn
    else manyTill' headerParser $ string "\r\n"
  let helperHeaders = if isJust helperHeadersIn then fromJust helperHeadersIn else []

  DT.trace "hoai" $ return ()
  body <- takeByteString
  --DT.trace ("got body " ++ (show body)) $ return ()


  -- Parse MIME if the message is in a MIME format
  let parsedBody = if isJust $ find isMIME headers
      then parseMIME (headers ++ helperHeaders) body
      else Right [TextBody $ decodeTextBody (headers ++ helperHeaders) body]

  let p = parseHeader headers
  let e2m = eitherToMaybe

  return $! parsedBody >>= \b -> return EmailMessage {
        origDate = e2m $ p parseTime "date"
      , from = e2m $ p parseEmailAddress "from"
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
    else (liftM . liftM) MIMEBody $ messageParser (Just headers) (Just bodyHeaders)

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
