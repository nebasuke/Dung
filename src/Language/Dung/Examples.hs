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
   AbsArg, a, b, c, exampleAF, exampleAF2, 
   -- * Example uses of the fixpoint definitions
   faf,
   -- * Example uses of the basic labelling definitions
   -- |Given @d = \"D\"@, @e = \"E\"@
   d, e, exampleAF3, exampleAF4,
   -- * Example uses of the input functionality
   exampleAF5,
   -- * Example uses of the output functionality
   output, output2, output3, output4, output5
  )
 where
import Language.Dung.AF
import Language.Dung.Input
import Language.Dung.Output
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
-- @[a,b]@ 'setAttacks' @c@ in the argumentation framework 'exampleAF':
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
-- The grounded labelling of the argumentation frameworks 'exampleAF'
-- and 'exampleAF2':
-- 
-- >>> grounded exampleAF
-- [("A",In),("C",In),("B",Out)]
-- 
-- >>> grounded exampleAF2
-- [("A",Undecided),("B",Undecided)]
--
-- [groundedExt:]
-- 
-- The grounded extension of the argumentation frameworks 'exampleAF'
-- and 'exampleAF2':
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
-- The grounded extension of the argumentation framework 'exampleAF' using the
-- fixpoint definition:
--
-- >>> groundedF faf
-- ["A","C"]
--
-- >>> groundedF (f exampleAF2)
-- []
faf :: [AbsArg] -> [AbsArg]
faf = f exampleAF

d, e :: AbsArg 
d = "D"
e = "E"

-- |Left hand side of Fig1. in Caminada.
-- Arguments are: {a,b,c,d}. 
-- Attacks: {(a, a), (a, c), (b, c), (c, d)}
exampleAF3 :: DungAF AbsArg
exampleAF3 = AF [a, b, c, d] [(a, a), (a, c), (b, c), (c, d)]

-- |Right hand side of Fig1. in Caminada.
-- Arguments are: {a,b,c,d,e}. 
-- Attacks: {(a, b), (b, a), (b, c), (c, d), (d, e), (e, c)}
--
-- [complete:]
-- 
-- The complete labellings of the argumentation framework 'exampleAF3'
-- and 'exampleAF4':
-- 
-- >>> complete exampleAF3
-- [
--   [("A",Undecided),("B",In),("C",Out),("D",In)]
-- ]
-- 
-- >>> complete exampleAF4
-- [
--   [("A",Out),("B",In),("C",Out),("D",In),("E",Out)],
--   [("A",In),("B",Out),("C",Undecided),("D",Undecided),("E",Undecided)],
--   [("A",Out),("B",In),("C",Out),("D",Undecided),("E",Undecided)]
-- ]
--
-- [completeExt:]
-- 
-- The complete extensions of the argumentation frameworks 'exampleAF3'
-- and 'exampleAF4':
--
-- >>> completeExt exampleAF3
-- [
--   ["B","D"]
-- ]
-- >>> completeExt exampleAF4
-- [
--   ["B","D"],
--   ["A"],
--   ["B"]
-- ]
--
-- [semiStable:]
-- 
-- The semi-stable labellings of the argumentation framework 'exampleAF3'
-- and 'exampleAF4':
-- 
-- >>> semiStable exampleAF3
-- [
--   [("A",Undecided),("B",In),("C",Out),("D",In)]
-- ]
-- 
-- >>> semiStable exampleAF4
-- [
--   [("A",Out),("B",In),("C",Out),("D",In),("E",Out)],
-- ]
--
-- [semiStableExt:]
-- 
-- The complete extensions of the argumentation frameworks 'exampleAF3'
-- and 'exampleAF4':
--
-- >>> semiStableExt exampleAF3
-- [
--   ["B","D"]
-- ]
-- >>> semiStableExt exampleAF4
-- [
--   ["B","D"],
-- ]
--
exampleAF4 :: DungAF AbsArg
exampleAF4 = AF [a, b, c, d, e] [(a, b), (b, a), (b, c), (c, d), (d, e), (e, c)]

-- |Parsed example as given on the CEGARTIX webpage:
-- <http://www.dbai.tuwien.ac.at/proj/argumentation/cegartix/>.
-- 
-- @
-- arg(a).
-- arg(b).
-- arg(c).
-- arg(d).
-- arg(e).
-- arg(f).
-- arg(g).
-- att(a,b).
-- att(c,b).
-- att(c,d).
-- att(d,c).
-- att(d,e).
-- att(e,g).
-- att(f,e).
-- att(g,f).
-- @
-- 
-- This is given as a literal string to 'parseAF'. 
exampleAF5 :: DungAF AbsArg
exampleAF5 = case 
  parseAF 
    "arg(a).\
    \arg(b).\
    \arg(c).\
    \arg(d).\
    \arg(e).\
    \arg(f).\
    \arg(g).\
    \att(a,b).\
    \att(c,b).\
    \att(c,d).\
    \att(d,c).\
    \att(d,e).\
    \att(e,g).\
    \att(f,e).\
    \att(g,f)."
      of 
  Left err -> error (show err)
  Right af -> af

-- |Output 'String' corresponding to 'exampleAF', 
-- i.e. @toCegartix exampleAF@.
--
-- >>> putStr output
-- arg("A").
-- arg("B").
-- arg("C").
-- att("A","B").
-- att("B","C").
output :: String
output = toCegartix exampleAF

-- |Output 'String' corresponding to 'exampleAF2', 
-- i.e. @toCegartix exampleAF2@.
--
-- >>> putStr output2
-- arg("A").
-- arg("B").
-- att("A","B").
-- att("B","A").
output2 :: String
output2 = toCegartix exampleAF2

-- |Output 'String' corresponding to 'exampleAF3', 
-- i.e. @toCegartix exampleAF3@.
--
-- >>> putStr output3
-- arg("A").
-- arg("B").
-- arg("C").
-- arg("D").
-- att("A","A").
-- att("A","C").
-- att("B","C").
-- att("C","D").
output3 :: String
output3 = toCegartix exampleAF3

-- |Output 'String' corresponding to 'exampleAF4', 
-- i.e. @toCegartix exampleAF4@.
--
-- >>> putStr output4
-- arg("A").
-- arg("B").
-- arg("C").
-- arg("D").
-- arg("E").
-- att("A","B").
-- att("B","A").
-- att("B","C").
-- att("C","D").
-- att("D","E").
-- att("E","C").
output4 :: String
output4 = toCegartix exampleAF4

-- |Output 'String' corresponding to 'exampleAF5', 
-- i.e. @toCegartix exampleAF5@.
output5 :: String
output5 = toCegartix exampleAF5

