module Network.Mail.Parse.Parsers.MIME where

import qualified Data.ByteString.Char8 as BSC

import Network.Mail.Parse.Parsers.Multipart (parseMultipart)
import Network.Mail.Parse.Parsers.Header (headerParser)
import Network.Mail.Parse.Decoders.BodyDecoder (decodeBody, decodeTextBody)
import Network.Mail.Parse.Parsers.Utils (discoverAttachment, isBroken)
import Network.Mail.Parse.Types
import Network.Mail.Parse.Utils

import Data.Either.Utils (maybeToEither)
import Data.Attoparsec.ByteString
import Data.List (find)
import Data.Maybe (fromJust, isJust)

import Data.Text.Encoding (encodeUtf8)
import Data.Either (rights, isRight)

import Codec.MIME.Parse (parseMIMEType)
import Codec.MIME.Type

import Control.Monad (foldM)

-- |Parses a MIME message part. Needs headers from the actual message
-- in case the MIME block misses some encoding blocks
mimeParser :: [Header] -> Parser EmailBody
mimeParser bodyHeaders = do
  headers <- manyTill' headerParser $ string "\r\n"
  body <- takeByteString

  let isAttachment = discoverAttachment headers
  if isJust isAttachment
    then do
      let filename = fromJust isAttachment
      let decodedBody = decodeBody headers body
      return $ Attachment headers filename (Just decodedBody) Nothing
    else return $! MIMEBody headers $ decodeTextBody (headers ++ bodyHeaders) body

-- |Parse a set of parts.
multipartParser :: [Header] -> [BSC.ByteString] -> Either ErrorMessage [EmailBody]
multipartParser bodyHeaders parts = do
  let parsed = map (parseOnly $ mimeParser bodyHeaders) parts
  foldM isBroken [] parsed

-- |Parse a mime encoded body.
parseMIME :: [Header] -> BSC.ByteString -> Either ErrorMessage [EmailBody]
parseMIME headers body = if isRight msgType then
  (case mimeType . head . rights $ [msgType] of
    Multipart _ -> multiParsed >>= multipartParser headers
    Text _ -> Right decodedBody
    _ -> Left "mimetype not supported")
  else Right decodedBody
  where msgType = findHeader "Content-Type" headers >>=
          Right . parseMIMEType . headerContents >>=
          maybeToEither "Couldn't parse message type"
        multiParsed = msgType >>=
          \x -> maybeToEither "asdads" $ find (\p -> paramName p == "boundary") (mimeParams x) >>=
          return . encodeUtf8 . paramValue >>=
          \b -> eitherToMaybe $ parseOnly (parseMultipart b) body
        decodedBody = [TextBody $ decodeTextBody headers body]
