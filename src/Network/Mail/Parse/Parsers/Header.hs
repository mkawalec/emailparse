module Network.Mail.Parse.Parsers.Header where

import Network.Mail.Parse.Types
import Network.Mail.Parse.Utils
import Network.Mail.Parse.Parsers.HeaderFields (parseText)

import Data.Word8
import Data.Attoparsec.ByteString
import qualified Data.Attoparsec.ByteString as AP
import Data.Text.Encoding (decodeUtf8)
import Data.Either.Unwrap (fromRight)
import Data.Either (isRight)

import qualified Data.ByteString.Char8 as BSC

-- |Parses a header
headerParser :: Parser Header
headerParser = do
  headerName <- AP.takeWhile (/= _colon)
  word8 _colon
  AP.takeWhile isWhitespace

  headerLine <- consumeTillEndLine
  moreLines <- many' isConsequentHeaderLine

  let headerBody = decodeUtf8 . cleanupLines $ headerLine:moreLines
  let parsedBody = parseText headerBody
  let body = if isRight parsedBody then fromRight parsedBody else headerBody

  return $ Header (decodeUtf8 headerName) body

-- |Concatenate lines insterting whitespace between them.
-- The whitespace needs to be inserted as these lines
-- come from parser that eats up the whitespace
cleanupLines :: [BSC.ByteString] -> BSC.ByteString
cleanupLines ls = BSC.intercalate " " $ map BSC.init ls
