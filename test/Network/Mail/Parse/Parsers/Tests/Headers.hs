module Network.Mail.Parse.Parsers.Tests.Headers (tests) where

import Test.Tasty (testGroup)
import Network.Mail.Parse.Parsers.Header (headerParser)
import Network.Mail.Parse.Types
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Attoparsec.ByteString as AP
import Data.Char (chr)
import Control.Monad (liftM)
import qualified Debug.Trace as DT

import Data.Either.Combinators (isRight, fromRight')

tests = testGroup "(checked by QuickCheck)" [
  (QC.testProperty "headerParser" prop_header)]

genChar :: Gen Char
genChar = do
  char <- choose (33, 126)
  if char == 58
    then genChar
    else return . chr $ char

shrinkText :: T.Text -> [T.Text]
shrinkText t = (tail $ T.tails t) ++ (init $ T.inits t)

genSomeText :: Int -> Gen [T.Text]
genSomeText maxSize = do
  testSize <- choose (0, maxSize)
  mapM (const $ T.singleton <$> genChar) [0..testSize]

genLines :: Int -> Int -> Gen T.Text
genLines maxLineLength maxLines = do
  lineLength :: Int <- choose (0, maxLineLength)
  linesNo :: Int <- choose (0, maxLines)

  allLines <- mapM (const $ liftM T.concat $ genSomeText lineLength) [0..linesNo]
  return $ T.intercalate "\r\n   " allLines

instance Arbitrary HeaderName where
  arbitrary = HeaderName . T.concat <$> genSomeText 7
  shrink (HeaderName t) = map HeaderName $ shrinkText t

instance Arbitrary HeaderValue where
  arbitrary = HeaderValue <$> genLines 10 4
  shrink (HeaderValue t) = map HeaderValue $ shrinkText t

newtype HeaderName = HeaderName T.Text
  deriving (Show, Eq, Ord)
newtype HeaderValue = HeaderValue T.Text
  deriving (Show, Eq, Ord)

prop_header :: HeaderName -> HeaderValue -> Property
prop_header (HeaderName hdrName) (HeaderValue hdrValue) = property $
    isRight parsed &&
    headerName unpackedParsed == hdrName &&
    headerContents unpackedParsed == hdrValue
  where headerLine = BSC.concat [encodeUtf8 hdrName, ": ",
                                 encodeUtf8 hdrValue, "\r\n"]
        parsed = AP.parseOnly headerParser headerLine
        unpackedParsed = fromRight' parsed
