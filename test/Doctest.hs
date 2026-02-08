module Main (main) where

import Test.DocTest

main :: IO ()
main = doctest
  [ "-isrc"
  , "-package", "containers"
  , "-package", "parsec"
  , "src/Language/Dung/Examples.hs"
  ]
