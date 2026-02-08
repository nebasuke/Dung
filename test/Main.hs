module Main (main) where

import Test.Tasty

import qualified Test.Language.Dung.AF as AF
import qualified Test.Language.Dung.Input as Input
import qualified Test.Language.Dung.Properties as Properties

main :: IO ()
main = defaultMain $ testGroup "Dung"
  [ AF.tests
  , Input.tests
  , Properties.tests
  ]
