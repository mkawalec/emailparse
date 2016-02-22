module Main where

import Test.Tasty (defaultMain, testGroup)

import Network.Mail.Parse.Parsers.Header (headerParser)
import Network.Mail.Parse.Types
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Attoparsec.ByteString as AP
import Data.Char (chr)

import Data.Either.Combinators (isRight, fromRight')

qcProps = testGroup "(checked by QuickCheck)" [
  (QC.testProperty "headerParser" prop_header)]

genChar :: Gen Char
genChar = do
  char <- choose (33, 126)
  if char == 58
    then genChar
    else return . chr $ char

instance Arbitrary HeaderName where
  arbitrary = HeaderName . T.concat <$> hdrText
    where char = genChar
          hdrText = mapM (const $ T.singleton <$> genChar) [1..5]
  shrink (HeaderName t) = map HeaderName $ (tail $ T.tails t) ++ (init $ T.inits t)

instance Arbitrary HeaderValue where
  arbitrary = HeaderValue . T.concat <$> hdrText
    where char = genChar
          hdrText = mapM (const $ T.singleton <$> genChar) [1..5]
  shrink (HeaderValue t) = map HeaderValue $ (tail $ T.tails t) ++ (init $ T.inits t)

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

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [qcProps]
