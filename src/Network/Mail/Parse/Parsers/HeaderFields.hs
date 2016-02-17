module Network.Mail.Parse.Parsers.HeaderFields (
  emailAddressParser,
  emailAddressListParser,
  parseTime,
  parseEmailAddress,
  parseEmailAddressList,
  parseText,
  parseTextList
) where

import Network.Mail.Parse.Types
import Network.Mail.Parse.Decoders.BodyDecoder (transferDecode, encodingToUtf)

import Data.Attoparsec.Text
import qualified Data.Text as T
import qualified Data.Text.Read as TR
import qualified Data.Attoparsec.Text as AP
import Data.Text.Encoding (encodeUtf8)
import Control.Applicative
import Data.Maybe
import qualified Data.Char as C
import Data.Either (isRight)
import Data.Either.Combinators (mapLeft, mapBoth)

import Data.Either.Unwrap (fromRight)

import Data.Time.Parse (strptime)
import Data.Time.LocalTime
import Control.Monad (join, liftM)

-- |Parses a name-addr formatted email
nameAddrParser :: Parser EmailAddress
nameAddrParser = do
  label <- AP.takeWhile (/= '<')
  _ <- char '<'

  address <- AP.takeWhile1 (/= '>')
  _ <- char '>'
  return $ EmailAddress address (Just . T.strip $ label)

-- |Parses an addr-spec formatted email
addrSpecParser :: Parser EmailAddress
addrSpecParser = do
  address <- AP.takeWhile1 (\c -> c /= '\r' && c /= ',' && c /= ' ')
  if isJust $ T.find (== '@') address
    then return $ EmailAddress address Nothing
    else fail "no @ in the address"

-- |Parses an email in any format
emailAddressParser :: Parser EmailAddress
emailAddressParser = nameAddrParser <|> addrSpecParser

-- |Eats unneeded whitespace
eatWhitespace :: Parser T.Text
eatWhitespace = AP.takeWhile (\c -> c == ',' || c == ' ')

-- |Parses a list of email addresses
emailAddressListParser :: Parser [EmailAddress]
emailAddressListParser =
  (eatWhitespace *> emailAddressParser) `sepBy'` char ','

minutesAndHoursToTZ :: Int -> Either T.Text (Int, T.Text) ->
  (Int, T.Text) -> Either T.Text TimeZone
minutesAndHoursToTZ direction minutes hours =
    Right $ minutesToTimeZone timezoneMins
  where knownMinutes  = if isRight minutes
                          then fst . fromRight $ minutes
                          else 0
        h             = fst hours
        timezoneMins  = direction * (h * 60 + knownMinutes)

zoneToOffset :: T.Text -> Either ErrorMessage TimeZone
zoneToOffset offset = if offsetH == '+' || offsetH == '-'
  then hours >>= (minutesAndHoursToTZ direction minutes)
  else Right $ minutesToTimeZone . (*60) $ case offset of
    "UT" -> 0
    "GMT" -> 0
    "EST" -> -5
    "EDT" -> -4
    "CST" -> -6
    "CDT" -> -5
    "MST" -> -7
    "MDT" -> -6
    "PST" -> -8
    "PDT" -> -7
    _ -> 0
  where offsetH = T.head offset
        direction = if offsetH == '+' then 1 else -1
        splitOffset = T.splitAt 2 $ T.tail offset
        hours = mapLeft T.pack $ TR.decimal . fst $ splitOffset
        minutes = mapLeft T.pack $ TR.decimal . snd $ splitOffset

timeToLocalTime :: (T.Text, T.Text, T.Text, [T.Text]) -> Maybe LocalTime
timeToLocalTime (day, month, year, timeOfDay@(hours:minutes:_)) =
    liftM fst (strptime "%d %b %Y %T" dateString)
  where dateString = T.intercalate " " [day, month, year, timeString]
        seconds = if length timeOfDay < 3 then "0" else last timeOfDay
        timeString = T.intercalate ":" [hours, minutes, seconds]

timeParser :: Parser (Either ErrorMessage ZonedTime)
timeParser =
  do
    day <- AP.takeWhile1 C.isDigit
    AP.takeWhile (== ' ')

    month <- AP.takeWhile1 C.isLetter
    AP.takeWhile (== ' ')

    year <- AP.takeWhile1 C.isDigit
    AP.takeWhile (== ' ')

    timeOfDay <- (AP.takeWhile (== ':') *> AP.takeWhile1 C.isDigit) `sepBy` char ':'

    AP.takeWhile (== ' ')
    zone <- AP.takeWhile1 (/= ' ')
    let localTime = timeToLocalTime (day, month, year, timeOfDay)
    let timeZone = zoneToOffset zone
    let result = if isJust localTime && isRight timeZone
                  then Right $ ZonedTime (fromJust localTime) (fromRight timeZone)
                  else mapBoth (const "cannot decode timezone") (const defaultZT) timeZone
    return result

-- |Parse a time from a header containing time
parseTime :: T.Text -> Either ErrorMessage ZonedTime
parseTime dateString = join . mapLeft T.pack $ parseOnly timeParser withoutDoW
  where withoutDoW = T.strip . last $  T.splitOn "," dateString

parseEmailAddress :: T.Text -> Either ErrorMessage EmailAddress
parseEmailAddress = mapLeft T.pack . parseOnly emailAddressParser

parseEmailAddressList :: T.Text -> Either ErrorMessage [EmailAddress]
parseEmailAddressList = mapLeft T.pack . parseOnly emailAddressListParser

untilEndSection :: Char -> Char -> Maybe Char
untilEndSection prev current =
  if prev == '?' && current == '='
    then Nothing
    else Just current

untilStartSection :: Char -> Char -> Maybe Char
untilStartSection prev current =
  if prev == '=' && current == '?'
    then Nothing
    else Just current

parseInlineEncoding :: Parser (Either ErrorMessage T.Text)
parseInlineEncoding = do
  charset <- AP.takeWhile1 (/= '?')
  char '?'

  encoding <- AP.takeWhile1 (/= '?')
  char '?'

  matchedText <- AP.scan ' ' untilEndSection
  char '='

  let text = encodeUtf8 $ T.init matchedText
  let decoded = mapLeft (const "Count not decode encoding") (transferDecode text encoding) >>= return . (`encodingToUtf` charset)

  if T.toLower encoding == "q"
    then return $ liftM (T.replace "_" " ") decoded
    else return decoded

parseTextBlock :: Parser (Either ErrorMessage T.Text)
parseTextBlock = do
  before <- AP.scan ' ' untilStartSection
  endReached <- atEnd
  decoded <- if not endReached
    then char '?' >> parseInlineEncoding
    else return . Right $ T.empty

  let didDecode = isRight decoded && (not . T.null . fromRight $ decoded)
  let normalizedBefore = if didDecode then T.init before else before
  return $ liftM (T.append normalizedBefore) decoded

untilEOF :: Parser (Either ErrorMessage T.Text) -> Parser [Either ErrorMessage T.Text]
untilEOF parser = do
  parsed <- parser
  endReached <- atEnd
  if endReached
    then return [parsed]
    else liftM (parsed:) (untilEOF parser)

parseText' :: Parser (Either ErrorMessage T.Text)
parseText' = do
  blocks <- untilEOF parseTextBlock
  return $ liftM T.concat (mapM id blocks)

parseText :: T.Text -> Either ErrorMessage T.Text
parseText = join . mapLeft T.pack . parseOnly parseText'

parseTextList :: T.Text -> T.Text -> Either ErrorMessage [T.Text]
parseTextList splitChar t = mapM parseText $ T.splitOn splitChar t
