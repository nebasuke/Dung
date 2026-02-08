module Test.Language.Dung.AF (tests) where

import Data.List (sort)
import Test.Tasty
import Test.Tasty.HUnit

import Language.Dung.AF

-- Convenience definitions matching Examples.hs
a, b, c, d, e :: String
a = "A"
b = "B"
c = "C"
d = "D"
e = "E"

-- A -> B -> C
exampleAF :: DungAF String
exampleAF = AF [a, b, c] [(a, b), (b, c)]

-- A <-> B
exampleAF2 :: DungAF String
exampleAF2 = AF [a, b] [(a, b), (b, a)]

-- Fig1 LHS in Caminada: {(a,a), (a,c), (b,c), (c,d)}
exampleAF3 :: DungAF String
exampleAF3 = AF [a, b, c, d] [(a, a), (a, c), (b, c), (c, d)]

-- Fig1 RHS in Caminada: {(a,b), (b,a), (b,c), (c,d), (d,e), (e,c)}
exampleAF4 :: DungAF String
exampleAF4 = AF [a, b, c, d, e] [(a, b), (b, a), (b, c), (c, d), (d, e), (e, c)]

tests :: TestTree
tests = testGroup "Language.Dung.AF"
  [ setAttacksTests
  , conflictFreeTests
  , acceptableTests
  , admissibleTests
  , groundedTests
  , groundedExtTests
  , groundedFTests
  , completeTests
  , completeExtTests
  , preferredExtTests
  , stableExtTests
  , semiStableTests
  , semiStableExtTests
  ]

setAttacksTests :: TestTree
setAttacksTests = testGroup "setAttacks"
  [ testCase "[a,b] attacks c in exampleAF" $
      setAttacks exampleAF [a, b] c @?= True
  , testCase "[b,c] does not attack a in exampleAF" $
      setAttacks exampleAF [b, c] a @?= False
  , testCase "[] does not attack b in exampleAF2" $
      setAttacks exampleAF2 [] b @?= False
  ]

conflictFreeTests :: TestTree
conflictFreeTests = testGroup "conflictFree"
  [ testCase "[a,c] is conflict-free in exampleAF" $
      conflictFree exampleAF [a, c] @?= True
  , testCase "[a,b,c] is not conflict-free in exampleAF" $
      conflictFree exampleAF [a, b, c] @?= False
  , testCase "[a,b] is not conflict-free in exampleAF2" $
      conflictFree exampleAF2 [a, b] @?= False
  ]

acceptableTests :: TestTree
acceptableTests = testGroup "acceptable"
  [ testCase "c acceptable w.r.t. [a,b] in exampleAF" $
      acceptable exampleAF c [a, b] @?= True
  , testCase "c not acceptable w.r.t. [] in exampleAF" $
      acceptable exampleAF c [] @?= False
  , testCase "b not acceptable w.r.t. [a,b,c] in exampleAF" $
      acceptable exampleAF b [a, b, c] @?= False
  ]

admissibleTests :: TestTree
admissibleTests = testGroup "admissible"
  [ testCase "[a,b,c] not admissible in exampleAF" $
      admissible exampleAF [a, b, c] @?= False
  , testCase "[a,c] is admissible in exampleAF" $
      admissible exampleAF [a, c] @?= True
  , testCase "[a] is admissible in exampleAF" $
      admissible exampleAF [a] @?= True
  ]

groundedTests :: TestTree
groundedTests = testGroup "grounded"
  [ testCase "grounded exampleAF" $
      sort (grounded exampleAF) @?= sort [("A", In), ("C", In), ("B", Out)]
  , testCase "grounded exampleAF2" $
      sort (grounded exampleAF2) @?= sort [("A", Undecided), ("B", Undecided)]
  ]

groundedExtTests :: TestTree
groundedExtTests = testGroup "groundedExt"
  [ testCase "groundedExt exampleAF" $
      sort (groundedExt exampleAF) @?= sort ["A", "C"]
  , testCase "groundedExt exampleAF2" $
      groundedExt exampleAF2 @?= []
  ]

groundedFTests :: TestTree
groundedFTests = testGroup "groundedF"
  [ testCase "groundedF (f exampleAF)" $
      sort (groundedF (f exampleAF)) @?= sort ["A", "C"]
  , testCase "groundedF (f exampleAF2)" $
      groundedF (f exampleAF2) @?= []
  , testCase "groundedF' (f exampleAF)" $
      sort (groundedF' (f exampleAF)) @?= sort ["A", "C"]
  , testCase "groundedF' (f exampleAF2)" $
      groundedF' (f exampleAF2) @?= []
  ]

completeTests :: TestTree
completeTests = testGroup "complete"
  [ testCase "complete exampleAF3 has one labelling" $
      length (complete exampleAF3) @?= 1
  , testCase "complete exampleAF3 content" $ case complete exampleAF3 of
      (lab:_) -> sort lab @?= sort [("A", Undecided), ("B", In), ("C", Out), ("D", In)]
      []    -> assertFailure "Expected at least one complete labelling"
  , testCase "complete exampleAF4 has three labellings" $
      length (complete exampleAF4) @?= 3
  ]

completeExtTests :: TestTree
completeExtTests = testGroup "completeExt"
  [ testCase "completeExt exampleAF3" $
      map sort (completeExt exampleAF3) @?= [sort ["B", "D"]]
  , testCase "completeExt exampleAF4 has three extensions" $
      length (completeExt exampleAF4) @?= 3
  , testCase "completeExt exampleAF4 content" $ do
      let exts = map sort (completeExt exampleAF4)
      sort ["B", "D"] `elem` exts @?= True
      sort ["A"] `elem` exts @?= True
      sort ["B"] `elem` exts @?= True
  ]

preferredExtTests :: TestTree
preferredExtTests = testGroup "preferredExt"
  [ testCase "preferredExt exampleAF" $
      map sort (preferredExt exampleAF) @?= [sort ["A", "C"]]
  , testCase "preferredExt exampleAF2" $ do
      let exts = map sort (preferredExt exampleAF2)
      length exts @?= 2
      ["A"] `elem` exts @?= True
      ["B"] `elem` exts @?= True
  ]

stableExtTests :: TestTree
stableExtTests = testGroup "stableExt"
  [ testCase "stableExt exampleAF" $
      map sort (stableExt exampleAF) @?= [sort ["A", "C"]]
  , testCase "stableExt exampleAF2" $ do
      let exts = map sort (stableExt exampleAF2)
      length exts @?= 2
      ["A"] `elem` exts @?= True
      ["B"] `elem` exts @?= True
  ]

semiStableTests :: TestTree
semiStableTests = testGroup "semiStable"
  [ testCase "semiStable exampleAF3 has one labelling" $
      length (semiStable exampleAF3) @?= 1
  , testCase "semiStable exampleAF3 content" $ case semiStable exampleAF3 of
      (s:_) -> sort s @?= sort [("A", Undecided), ("B", In), ("C", Out), ("D", In)]
      []    -> assertFailure "Expected at least one semi-stable labelling"
  , testCase "semiStable exampleAF4 has one labelling" $
      length (semiStable exampleAF4) @?= 1
  , testCase "semiStable exampleAF4 content" $ case semiStable exampleAF4 of
      (s:_) -> sort s @?= sort [("A", Out), ("B", In), ("C", Out), ("D", In), ("E", Out)]
      []    -> assertFailure "Expected at least one semi-stable labelling"
  ]

semiStableExtTests :: TestTree
semiStableExtTests = testGroup "semiStableExt"
  [ testCase "semiStableExt exampleAF3" $
      map sort (semiStableExt exampleAF3) @?= [sort ["B", "D"]]
  , testCase "semiStableExt exampleAF4" $
      map sort (semiStableExt exampleAF4) @?= [sort ["B", "D"]]
  ]
