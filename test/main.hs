module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Network.Mail.Parse.Parsers.Tests.Headers as TH
import qualified Network.Mail.Parse.Parsers.Tests.HeaderFields as THF

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [TH.tests, THF.tests]
