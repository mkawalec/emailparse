module Network.Mail.Parse.Utils where

import Network.Mail.Parse.Types

import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import qualified Data.ByteString.Char8 as BS
import Data.Word8
import Data.List

import Data.Text (Text)
import qualified Data.Text as T
import Data.Either.Utils (maybeToEither)

-- |If the previous character was a carriage return and the current
-- is a line feed, stop parsing
hadCRLF :: Word8 -> Word8 -> Maybe Word8
hadCRLF prev current =
  if prev == _cr && current == _lf
    then Nothing
    else Just current

-- |Consumes a line until CRLF is hit
consumeTillEndLine :: Parser BS.ByteString
consumeTillEndLine = scan 0 hadCRLF <* satisfy (== _lf)

-- |Can a given character be regarded as a whitespace?
isWhitespace :: Word8 -> Bool
isWhitespace x = x == 9 || x == 32

-- |If the next line is a part of a previous header, parse it.
-- Fail otherwise
isConsequentHeaderLine :: Parser BS.ByteString
isConsequentHeaderLine = satisfy isWhitespace *>
                         AP.takeWhile isWhitespace *>
                         consumeTillEndLine

-- |Remove a MIME header comment and return a header without the comment
commentRemover :: Text -> Text
commentRemover contents = T.strip withoutComment
  where splitAtComment = T.split (\c -> c == '(' || c == ')') contents
        withoutComment = case splitAtComment of
            [] -> ""
            [x] -> x
            x:xs -> T.append x (last xs)

-- |Given a header name, it will try to locate it in
-- a list of headers, fail if it's not there
findHeader :: Text -> [Header] -> Either ErrorMessage Header
findHeader hdr headers = maybeToEither notFound header
  where notFound    = T.concat ["Cound not find header '", T.pack . show $ hdr, "'"]
        eigenHeader = T.toLower hdr
        header      = find (\x -> T.toLower (headerName x) == eigenHeader) headers

eitherToMaybe :: Either a b -> Maybe b
eitherToMaybe (Right a) = Just a
eitherToMaybe (Left _) = Nothing
