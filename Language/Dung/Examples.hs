-- | This is the examples module accompanying the implementation of Dung's 
-- argumentation frameworks. 
--
-- This module contains a collection of examples, showing how to define 
-- arguments, argumentation frameworks and how to use the standard definitions.
--
-- To run these examples, or your own: start GHCi and do the following:
--
-- @\:l Language.Dung.Examples@
-- 
module Language.Dung.Examples 
  (
   -- * Example uses of the basic definitions 
   -- |Given @a = \"A\"@, @b = \"B\"@, @c = \"C\"@
   AbsArg, exampleAF, exampleAF2,
   -- * Example uses of the fixpoint definitions
   faf)
 where
import Language.Dung.AF

-- | The simplest abstract argument is an argument identifiable by its name
type AbsArg = String 


a, b, c :: AbsArg 
a = "A"
b = "B"
c = "C"


-- |Example AF: A -> B -> C 
exampleAF :: DungAF AbsArg
exampleAF = AF [a, b, c] [(a, b), (b, c)]

-- |Example AF: A \<-> B
--
-- Now follow a few example outputs using the above argumentation frameworks.
--
-- [setAttacks:]
-- 
--  @[a,b]@ 'setAttacks' @c@ in the argumentation framework 'exampleAF':
-- 
-- >>> setAttacks exampleAF [a,b] c
-- True
--
-- >>> setAttacks exampleAF [b,c] a
-- False
-- 
-- >>> setAttacks exampleAF2 [] b
-- False
--
-- [conflictFree:]
--
-- @\[a,c\]@ is 'conflictFree' in the argumentation framework 'exampleAF':
-- 
-- >>> conflictFree exampleAF [a,c]
-- True
--
-- >>> conflictFree exampleAF [a,b,c]
-- False
--
-- >>> conflictFree exampleAF2 [a,b]
-- False
-- 
-- [acceptable:]
--
-- @c@ is acceptable w.r.t. @\[a,b\]@ in the argumentation framework 'exampleAF':
--
-- >>> acceptable exampleAF c [a,b]
-- True
-- 
-- >>> acceptable exampleAF c [] 
-- False
--
-- >>> acceptable exampleAF b [a,b,c] 
-- False
-- 
-- [admissible:]
-- 
-- @\[a,b,c\]@ is admissible in the argumentation framework 'exampleAF':
--
-- >>> admissible exampleAF [a,b,c]
-- False
-- 
-- >>> admissible exampleAF [a,c]
-- True
-- 
-- >>> admissible exampleAF [a]
-- True
--
-- [grounded:]
-- 
-- The grounded labelling of the argumentation framework 'exampleAF':
--
-- >>> grounded exampleAF
-- [("A",In),("C",In),("B",Out)]
-- 
-- >>> grounded exampleAF2
-- [("A",Undecided),("B",Undecided)]
--
-- [groundedExt:]
-- 
-- The grounded extension of the argumentation framework 'exampleAF':
--
-- >>> groundedExt exampleAF
-- ["A", "C"]
-- >>> groundedExt exampleAF2
-- []
exampleAF2 :: DungAF AbsArg 
exampleAF2 = AF [a, b] [(a, b), (b, a)]

-- |fixed point function for a specific argumentation framework,
-- @faf = f exampleAF@.
-- 
-- [groundedF:]
--
-- The grounded extension of the argumentation framework 'exampleAF' using the fixpoint definition:
--
-- >>> groundedF faf
-- ["A","C"]
--
-- >>> groundedF (f exampleAF2)
-- []
faf :: [AbsArg] -> [AbsArg]
faf = f exampleAF

