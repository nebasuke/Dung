module Test.Language.Dung.Input (tests) where

import Test.Tasty
import Test.Tasty.HUnit

import Language.Dung.AF
import Language.Dung.Input
import Language.Dung.Output

tests :: TestTree
tests = testGroup "Language.Dung.Input"
  [ parseTests
  , roundTripTests
  , edgeCaseTests
  ]

-- Helper to unwrap a Right or fail the test
expectRight :: Show a => Either a b -> IO b
expectRight (Right x) = return x
expectRight (Left err) = assertFailure ("Expected Right but got Left: " ++ show err)

parseTests :: TestTree
parseTests = testGroup "parseAF"
  [ testCase "parses single argument" $ do
      af <- expectRight $ parseAF "arg(a)."
      af @?= AF ["a"] []
  , testCase "parses argument and attack" $ do
      af <- expectRight $ parseAF "arg(a). arg(b). att(a,b)."
      af @?= AF ["a", "b"] [("a", "b")]
  , testCase "parses atk syntax" $ do
      af <- expectRight $ parseAF "arg(a). arg(b). atk(a,b)."
      af @?= AF ["a", "b"] [("a", "b")]
  , testCase "parses multi-line input" $ do
      let input = unlines
            [ "arg(a)."
            , "arg(b)."
            , "arg(c)."
            , "att(a,b)."
            , "att(b,c)."
            ]
      af <- expectRight $ parseAF input
      af @?= AF ["a", "b", "c"] [("a", "b"), ("b", "c")]
  , testCase "parses exampleaf.txt format" $ do
      let input = "arg(a). arg(b). arg(c). arg(d). arg(e). arg(f). arg(g). att(a,b). att(c,b). att(c,d). att(d,c). att(d,e). att(e,g). att(f,e). att(g,f)."
      af <- expectRight $ parseAF input
      af @?= AF ["a","b","c","d","e","f","g"]
                [("a","b"),("c","b"),("c","d"),("d","c"),("d","e"),("e","g"),("f","e"),("g","f")]
  , testCase "returns Left on invalid input" $ do
      let result = parseAF "invalid input"
      case result of
        Left _  -> return ()
        Right _ -> assertFailure "Expected parse error"
  ]

roundTripTests :: TestTree
roundTripTests = testGroup "round-trip"
  [ testCase "parse . toCegartix identity for simple AF" $ do
      let af = AF ["a", "b", "c"] [("a", "b"), ("b", "c")] :: DungAF String
          output = toCegartix af
      case parseAF output of
        Left err  -> assertFailure $ "Parse error: " ++ show err
        Right af' -> af' @?= af
  , testCase "parse . toCegartix identity for self-attacking AF" $ do
      let af = AF ["a", "b"] [("a", "a"), ("a", "b")] :: DungAF String
          output = toCegartix af
      case parseAF output of
        Left err  -> assertFailure $ "Parse error: " ++ show err
        Right af' -> af' @?= af
  ]

edgeCaseTests :: TestTree
edgeCaseTests = testGroup "edge cases"
  [ testCase "single argument, no attacks" $ do
      af <- expectRight $ parseAF "arg(x)."
      af @?= AF ["x"] []
  , testCase "string literal argument names" $ do
      af <- expectRight $ parseAF "arg(\"hello\"). arg(\"world\"). att(\"hello\", \"world\")."
      af @?= AF ["hello", "world"] [("hello", "world")]
  ]
