module Main where

import Test.Tasty (defaultMain, testGroup)
import qualified Network.Mail.Parse.Parsers.Tests.Headers as TH

main :: IO ()
main = defaultMain $ testGroup "Tests"
  [TH.tests]
