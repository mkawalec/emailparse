module Network.Mail.Parse.Parsers.Tests.HeaderFields (tests) where

import Network.Mail.Parse.Types
import Network.Mail.Parse.Parsers.HeaderFields (emailAddressParser)

import Test.Tasty (testGroup)
import Test.Tasty.QuickCheck as QC
import Test.QuickCheck
import qualified Data.Attoparsec.Text as AP
import qualified Data.ByteString.Char8 as BSC
import qualified Data.Text as T
import Control.Monad (liftM)
import Data.Char (chr)

import Data.Either.Combinators (isRight, fromRight')

tests = testGroup "HeaderFields" [
  (QC.testProperty "email parser" prop_email)]


simpleEmailChars = [65..90] ++ [97..122] ++ [48..57] ++ [33] ++
  [35..39] ++ [42, 43, 45, 47, 61, 63] ++ [94..96] ++ [123..126]

domainPartChars = [65..90] ++ [97..122] ++ [48..57] ++ [45, 46]

-- TODO: Support quoted strings
-- TODO: Support comments
-- TODO: Support emails with a name

genChar :: [Int] -> Gen T.Text
genChar range = do
  char <- liftM chr $ QC.elements range
  return $ T.singleton char

genPlainEmail :: Gen T.Text
genPlainEmail = do
  localPartLength :: Int <- QC.choose (1, 300)
  domainPartLength :: Int <- QC.choose (3, 20)

  localPart <- liftM T.concat $ mapM (const $ genChar simpleEmailChars) [1..localPartLength]
  domainPart <- liftM T.concat $ mapM (const $ genChar domainPartChars) [1..domainPartLength]

  return $ T.concat [localPart, "@", domainPart]

newtype TestEmail = TestEmail T.Text
  deriving (Eq, Ord, Show)

instance Arbitrary TestEmail where
  arbitrary = liftM TestEmail genPlainEmail
  shrink _ = []

prop_email :: TestEmail -> Property
prop_email (TestEmail email) = property $
    isRight parsed &&
    emailAddress unpackedParsed == email
  where parsed = AP.parseOnly emailAddressParser email
        unpackedParsed = fromRight' parsed
