module Test.Language.Dung.Properties (tests) where

import Data.List (sort)
import qualified Data.Set as Set
import Test.Tasty
import Test.Tasty.QuickCheck

import Language.Dung.AF

-- Generate small AFs with 3-5 arguments for tractable computation
newtype SmallAF = SmallAF (DungAF String)
  deriving (Show)

instance Arbitrary SmallAF where
  arbitrary = do
    n <- choose (3, 5 :: Int)
    let args = map (\i -> "a" ++ show i) [1..n]
    numAtks <- choose (0, n * n `div` 2)
    atks <- vectorOf numAtks $ do
      from <- elements args
      to   <- elements args
      return (from, to)
    -- Remove duplicate attacks
    let uniqueAtks = Set.toList . Set.fromList $ atks
    return $ SmallAF (AF args uniqueAtks)

-- Helper: check if a set is a subset of another
isSubsetOf :: Ord a => [a] -> [a] -> Bool
isSubsetOf xs ys = Set.fromList xs `Set.isSubsetOf` Set.fromList ys

tests :: TestTree
tests = testGroup "QuickCheck Properties"
  [ groundedProperties
  , preferredProperties
  , stableProperties
  , semanticRelationProperties
  ]

groundedProperties :: TestTree
groundedProperties = testGroup "grounded"
  [ testProperty "grounded extension is conflict-free" $ \(SmallAF af) ->
      conflictFree af (groundedExt af)
  , testProperty "grounded extension is admissible" $ \(SmallAF af) ->
      admissible af (groundedExt af)
  , testProperty "grounded labelling covers all arguments" $ \(SmallAF af@(AF args _)) ->
      sort (map fst (grounded af)) == sort args
  ]

preferredProperties :: TestTree
preferredProperties = testGroup "preferred"
  [ testProperty "every preferred extension is admissible" $ \(SmallAF af) ->
      all (admissible af) (preferredExt af)
  , testProperty "every preferred extension is conflict-free" $ \(SmallAF af) ->
      all (conflictFree af) (preferredExt af)
  , testProperty "at least one preferred extension exists" $ \(SmallAF af) ->
      not (null (preferredExt af))
  ]

stableProperties :: TestTree
stableProperties = testGroup "stable"
  [ testProperty "every stable extension is also a preferred extension" $ \(SmallAF af) ->
      let stExts = map sort (stableExt af)
          prExts = map sort (preferredExt af)
      in all (`elem` prExts) stExts
  , testProperty "every stable extension is conflict-free" $ \(SmallAF af) ->
      all (conflictFree af) (stableExt af)
  ]

semanticRelationProperties :: TestTree
semanticRelationProperties = testGroup "semantic relations"
  [ testProperty "grounded is the smallest complete extension" $ \(SmallAF af) ->
      let gExt = sort (groundedExt af)
          cExts = completeExt af
      in all (\ce -> gExt `isSubsetOf` ce) cExts
  , testProperty "every complete extension is admissible" $ \(SmallAF af) ->
      all (admissible af) (completeExt af)
  , testProperty "groundedF agrees with groundedExt" $ \(SmallAF af) ->
      sort (groundedF (f af)) == sort (groundedExt af)
  , testProperty "groundedF' agrees with groundedExt" $ \(SmallAF af) ->
      sort (groundedF' (f af)) == sort (groundedExt af)
  ]
