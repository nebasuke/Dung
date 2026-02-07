{-# OPTIONS_GHC -Wno-name-shadowing #-}
-- | This module implements Dung's argumentation frameworks.
module Language.Dung.AF
 (
    -- * Basic definitions  
   DungAF(..), 
   setAttacks, aplus, amin, argplus, argmin, 
   conflictFree, acceptable, f, admissible, 
   -- * Grounded, complete, preferred and stable semantics through fixpoints
   groundedF, groundedF', completeF, preferredF, stableF,
   -- * Definitions of a preferred and stable extension
   isPreferredExt, isStableExt,
   -- * Basic labelling definitions
   -- |The following functions are implementations of the 
   -- definitions in \"An algorithm for Computing Semi-Stable 
   -- Semantics\" in \"Symbolic and Quantitative Approaches to Reasoning with
   -- Uncertainty\", pages 222--234, Springer, 2007.
   Status(..), Labelling,
   inLab, outLab, undecLab, 
   allIn, allOut, allUndec,
   powerLabel,
   unattacked, attacked, 
   labAttackers, illegallyIn, illegallyOut, illegallyUndec,
   legallyIn, legallyOut, legallyUndec,
   isAdmissible, isComplete, isGrounded, isPreferred, isStable, isSemiStable,
   transitionStep, terminatedTransition, superIllegallyIn,
   -- * Grounded, preferred, semi-stable and stable labellings
   -- |The following functions are implementations of the 
   -- definitions in \"An algorithm for Computing Semi-Stable 
   -- Semantics\" in \"Symbolic and Quantitative Approaches to Reasoning with 
   -- Uncertainty\", pages 222--234, Springer, 2007 and Section 4.1 of Proof 
   -- Theories and Algorithms for Abstract Argumentation Frameworks by Modgil 
   -- and Caminada.
   grounded, groundedExt, complete, preferred, stable, semiStable, 
   completeExt, preferredExt, stableExt, semiStableExt
 )
 where
import Data.List (partition, delete, sort)
-- For the implementation of intersect, (\\) and nub
import qualified Data.Set as Set
import qualified Data.Map.Strict as Map
-- import Prelude hiding ((\\))

-- Haskell library's intersect, (\\) and nub only require an Eq instance.
-- If we have an Ord instance as well, it can be sped up significantly.
-- I therefore use intersect, (\\) and nub from https://github.com/nh2/haskell-ordnub 
-- by Niklas Hambuechen
intersect :: (Ord a) => [a] -> [a] -> [a]
intersect a b = filter (`Set.member` bSet) a
  where
    bSet = Set.fromList b

nub :: (Ord a) => [a] -> [a]
nub = go Set.empty
  where
    go _ [] = []
    go s (x:xs) = if x `Set.member` s then go s xs
                                      else x : go (Set.insert x s) xs

infix 5 \\

(\\) :: (Ord a) => [a] -> [a] -> [a]
a \\ b = go initHist a
  where
    initHist = Map.fromListWith (+) [ (x, 1 :: Int) | x <- b ]

    go _    []     = []
    go hist (x:xs) = case Map.lookup x hist of
      Just n | n > 0 ->     go (Map.insert x (n-1) hist) xs
      _              -> x : go hist                      xs

-- |An abstract argumentation framework is a set of arguments 
-- (represented as a list) and an attack relation on these arguments. 
data DungAF arg = AF [arg] [(arg, arg)]
  deriving (Eq, Ord, Show)

-- |Given an argumentation framework, determines whether args 
-- (subset of the arguments in the AF), attacks an argument arg (in the AF).
setAttacks :: Eq arg => DungAF arg -> [arg] -> arg -> Bool
setAttacks (AF _ def) args arg 
  = or [b == arg | (a, b) <- def, a `elem` args] 

-- |Given an argumentation framework, determines the set of arguments
-- that are attacked by an argument (in the AF).
aplus :: Eq arg => DungAF arg -> arg -> [arg]
aplus (AF _args atk) a = [b | (a', b) <- atk, a == a']

-- |Given an argumentation framework, determines the set of arguments
-- attacking an argument (in the AF).
amin :: Eq arg => DungAF arg -> arg -> [arg]
amin (AF _args atk) a = [b | (b, a') <- atk, a == a']

-- |Given an argumentation framework, determines the set of arguments
-- that are attacked by the given subset of arguments (in the AF).
argplus :: Ord arg => DungAF arg -> [arg] -> [arg]
argplus af = nub . concatMap (aplus af)

-- |Given an argumentation framework, determines the set of arguments
-- that attack a given subset of arguments (in the AF).
argmin :: Ord arg => DungAF arg -> [arg] -> [arg]
argmin af = nub . concatMap (amin af)

-- |Given an argumentation framework, determines whether args 
-- (subset of the arguments in the AF) is conflict-free.
conflictFree :: Eq arg => DungAF arg -> [arg] -> Bool
conflictFree (AF _ def) args 
  = null [(a,b) | (a, b) <- def, a `elem` args, b `elem` args] 

-- |Given an argumentation framework, determines whether an  
-- argument is acceptable with respect to a list of 'args' (subset of the arguments in the AF). 
acceptable :: Eq arg => DungAF arg -> arg -> [arg] -> Bool
acceptable af@(AF _ def) a args 
  = and [setAttacks af args b | (b, a') <- def, a == a']

-- |Given an argumentation framework, returns the set of arguments  
-- that are acceptable with respect to 'args' (subset of the arguments in the AF). 
f :: Eq arg => DungAF arg -> [arg] -> [arg]
f af@(AF args' _) args = [a | a <- args', acceptable af a args]  

-- Returns 'True' if 'xs' is a subset of 'ys'
subset :: Ord a => [a] -> [a] -> Bool
xs `subset` ys = null (xs \\ ys)

-- |Given an argumentation framework, determines whether 
-- the set of arguments 'args' (subset of the arguments in the AF) is admissible,
-- i.e. if 'args' is 'conflictFree' and args is a subset of @f af args@
admissible :: Ord arg =>  DungAF arg -> [arg] -> Bool
admissible af args = conflictFree af args && args `subset` f af args 

-- alternatively: 
-- if 'args' is 'conflictFree' and each argument in args is acceptable with
-- respect to args. 
-- admissible af args = conflictFree af args && 
--                      and [acceptable af arg args | arg <- args]

-------------------------------------------------------------------------------
--- Implementations of semantics through fixpoints or generation of complete--- 
---                              extensions                                 ---
-------------------------------------------------------------------------------

-- |Given a characteristic function f, computes the grounded extension
-- by iterating on the empty set (list) until it reaches a fixpoint.
groundedF :: Eq arg => ([arg] -> [arg]) -> [arg]
groundedF f = step f []
  where  step f args 
           | f args == args  = args
           | otherwise       = step f (f args)

-- |Given a characteristic function f, computes the grounded extension
-- by iterating on the empty set (list) until it reaches a fixpoint.
-- Strict version.
groundedF'  :: Eq arg => ([arg] -> [arg]) -> [arg]
groundedF' f = step f []
  where  step f args 
           | f args == args  = args
           | otherwise       = 
            let args' = f args
            in args' `seq` step f args'
                        
-- Computes the powerset of a list.
powerset :: [a] -> [[a]]
powerset []     = [[]]
powerset (x:xs) = powerset xs ++ map (x:) (powerset xs)

-- |Given an argumentation framework, computes all complete extension, 
-- by taking all sets of arguments of the powerset of arguments of that AF, 
-- given that they are admissible and @f af == f@.
completeF :: Ord arg => DungAF arg -> [[arg]]
completeF af@(AF args _) = 
  let fAF = f af 
  in  filter (\ x -> admissible af x && x == fAF x) (powerset args)

-- |Given an argumentation framework, computes all preferred extensions,
-- by applying a filter on the complete extensions. Note that this, 
-- naive definition is faster than the current algorithm implementation.
preferredF :: Ord arg => DungAF arg -> [[arg]]
preferredF af =
  let cs = completeF af
  in filter (isPreferredExt af cs) cs

-- |Given an argumentation framework, computes all stable extensions,
-- by applying a filter on the complete extensions. Note that this, 
-- naive definition is faster than the current algorithm implementation.
stableF :: Ord arg => DungAF arg -> [[arg]]
stableF af =
  let ps = preferredF af
  in  filter (isStableExt af) ps

-- |A complete extension is also a preferred extension if it is not a 
-- subset of one of the other extensions. 
isPreferredExt :: Ord arg => DungAF arg -> [[arg]] -> [arg] -> Bool
isPreferredExt _af exts ext = all (not . (ext `subset`))
                                 (delete ext exts)

-- |S is a stable extension is an extension iff it is equal to the set 
-- of arguments not attacked by S.
isStableExt :: Ord arg => DungAF arg -> [arg] -> Bool 
isStableExt af@(AF args _) ext = filter (unattacked (args \\ ext) af) args == ext

-------------------------------------------------------------------------
-- The following functions are implementations of the 
-- definitions in \"An algorithm for Computing Semi-Stable 
-- Semantics\" in \"Symbolic and Quantitative Approaches to 
-- Reasoning with Uncertainty\", pages 222--234, Springer, 2007.
-------------------------------------------------------------------------------

-- |Labelling status of arguments.
data Status = In | Out | Undecided
  deriving (Eq, Show, Ord)

-- Definition 4
-- |Labelling of arguments. 
type Labelling arg = [(arg,Status)]


-- Just below Definition 4, functions on a labelling:
-- in(Lab)
-- |Given a labelling of arguments, give back the arguments labelled 'In'.
inLab :: Labelling arg -> [arg]
inLab labs = [a | (a, In) <- labs]

-- out(Lab)
-- |Given a labelling of arguments, give back the arguments labelled 'Out'.
outLab :: Labelling arg -> [arg]
outLab labs = [a | (a, Out) <- labs]

-- undec(lab)
-- |Given a labelling of arguments, give back the arguments labelled 
-- 'Undecided'.
undecLab :: Labelling arg -> [arg]
undecLab labs = [a | (a, Undecided) <- labs]


-- Just below Definition 4, Caminada distinguishes three special kinds of labelling.

-- |The allIn labelling is a 'Labelling' that labels every argument 'In'.
allIn :: [arg] -> Labelling arg
allIn = map (\ a -> (a, In))

-- |The allOut labelling is a 'Labelling' that labels every argument 'Out'.
allOut :: [arg] -> Labelling arg
allOut = map (\ a -> (a, Out))

-- |The allUndec labelling is a 'Labelling' that labels every argument 'Undecided'.
allUndec :: [arg] -> Labelling arg
allUndec = map (\ a -> (a, Undecided))

-- |Given a list of arguments that are 'Out' in an argumentation framework af, 
-- an argument 'arg' is unattacked if the list of its attackers, ignoring the outs, is empty. 
unattacked :: Ord arg => [arg] -> 
              DungAF arg -> arg -> Bool
unattacked outs (AF _ def) arg = 
  let attackers = [a | (a, b) <- def, arg == b]
  in null (attackers \\ outs)

-- |Given a list of arguments that are 'In' in an argumentation framework af, 
-- an argument 'arg' is attacked if there exists an attacker that is 'In'.
attacked :: Ord arg => [arg] -> 
            DungAF arg -> arg -> Bool
attacked ins (AF _ def) arg = 
  let attackers = [a | (a, b) <- def, arg == b]
  in not (null (attackers `intersect` ins))

-- |Computes a list with all possible labellings.
powerLabel :: [arg] -> [Labelling arg]
powerLabel []     = [[]]
powerLabel (x:xs) = map ((x,In):)        (powerLabel xs) 
                 ++ map ((x,Out):)       (powerLabel xs) 
                 ++ map ((x,Undecided):) (powerLabel xs)
------  

-- |Computes the grounded labelling for a Dung argumentation framework,
-- returning a (unique) list of arguments with statuses.
-- 
-- Based on section 4.1 of Proof Theories and Algorithms for Abstract Argumentation Frameworks
-- by Modgil and Caminada.
grounded :: Ord arg => DungAF arg -> Labelling arg
grounded af@(AF args _) = grounded' [] [] args af
 where 
 grounded' :: Ord a => [a] -> [a] -> 
              [a] -> DungAF a -> [(a, Status)]
 grounded' ins outs [] _   
  =    allIn ins 
    ++ allOut outs
 grounded' ins outs args af  = 
   let newIns  = filter (unattacked outs af) args
       newOuts = filter (attacked ins af) args
   in if null (newIns ++ newOuts) 
      then allIn ins
        ++ allOut outs 
        ++ allUndec args
      else grounded' (ins ++ newIns) 
                     (outs ++ newOuts) 
                     (args \\ (newIns ++ newOuts)) 
                     af

-- |The grounded extension of an argumentation framework is just the grounded labelling, 
-- keeping only those arguments that were labelled 'In'.
groundedExt :: Ord arg => DungAF arg -> [arg]
groundedExt af = [arg | (arg, In) <- grounded af] 

-- |Given an argumentation framework, determines the list of attackers of an argument, 
-- from a given labelling, returning the labelled attackers. 
labAttackers :: Eq arg => DungAF arg -> arg -> Labelling arg -> Labelling arg
labAttackers (AF _args atk) a labs = [lab | lab@(b, _) <- labs, (b, a) `elem` atk]

-- Definition 5.1 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is illegally 'In' iff a is labelled 'In',
-- but not all its attackers are labelled 'Out'.
illegallyIn :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
illegallyIn af labs (a, In) = not . null $ [()|  (_, l) <- labAttackers af a labs, l /= Out]
illegallyIn _  _     _      = False

-- Definition 5.2 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is illegally 'Out' iff a is labelled 'Out'
-- but does not have an attacker labelled 'In'.
illegallyOut :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
illegallyOut af labs (a, Out) = null [() | (_, In) <- labAttackers af a labs]
illegallyOut _  _    _        = False

-- Definition 5.3 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is illegally 'Undecided' iff a is labelled 'Undecided' 
-- but either all its attackers are labelled 'Out' 
-- or it has an attacker that is labelled 'In'.
illegallyUndec :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
illegallyUndec af labs (a, Undecided) = and [l == Out | (_, l) <- labAttackers af a labs]
                                        || (not . null) [() | (_, In) <- labAttackers af a labs]
illegallyUndec _  _    _              = False


-- Just below Definition 5.3 of Caminada
-- The implementation of a 'Labelling' that has no illegal
-- arguments is given as 'isComplete', further below.

-- Just below Definition 5.3 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is legally 'In' iff a is labelled 'In' 
-- and it's not 'illegallyIn'.
legallyIn :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
legallyIn af labs arg@(_, In) = not $ illegallyIn af labs arg
legallyIn _  _    _           = False

-- Just below Definition 5.3 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is legally 'Out' iff a is labelled 'Out' 
-- and it's not 'illegallyOut'.
legallyOut :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
legallyOut af labs arg@(_, Out) = not $ illegallyOut af labs arg
legallyOut _  _    _            = False

-- Just below Definition 5.3 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is legally 'Undecided' iff a is labelled 'Undecided' 
-- and it's not 'illegallyUndec'.
legallyUndec :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
legallyUndec af labs arg@(_, Undecided) = not $ illegallyUndec af labs arg
legallyUndec _  _    _                  = False

-- Definition 6 of Caminada
-- |Given an AF, an admissible labelling is a 'Labelling' without arguments
-- that are 'illegallyIn' and without arguments that are 'illegallyOut'.
isAdmissible :: Eq arg => DungAF arg -> Labelling arg -> Bool
isAdmissible af labs = null $
                      [lab | lab@(_, In) <- labs, illegallyIn af labs lab]
                   ++ [lab | lab@(_, Out) <- labs, illegallyOut af labs lab] 

-- Definition 7 of Caminada
-- |Given an AF, a complete labelling is a labelling without arguments
-- that are 'illegallyIn', without arguments that are 'illegallyOut' and 
-- without arguments that are 'illegallyUndec'.
isComplete ::  Eq arg => DungAF arg -> Labelling arg -> Bool
isComplete af labs = null $
                   [lab | lab@(_, In) <- labs, illegallyIn af labs lab]
                ++ [lab | lab@(_, Out) <- labs, illegallyOut af labs lab]
                ++ [lab | lab@(_, Undecided) <- labs, illegallyUndec af labs lab]


-- Definition 8 of Caminada, grounded labelling
-- |Let 'labs' be a complete labelling, i.e. @isComplete af labs@, we say that 
-- labs is a grounded labelling iff @inLab labs@ is minimal 
-- (w.r.t. set inclusion).
isGrounded :: Ord arg => DungAF arg -> [Labelling arg] -> Labelling arg -> Bool
isGrounded af labss labs = isComplete af labs && 
                           all (inLab labs `subset`) (map inLab labss)

-- Definition 8 of Caminada, preferred labelling
-- |Let 'labs' be a complete labelling, i.e. @isComplete af labs@, we say that 
-- labs is a preferred labelling iff @inLab labs@ is maximal 
-- (w.r.t. set inclusion).
isPreferred :: Ord arg => DungAF arg -> [Labelling arg] -> Labelling arg -> Bool
isPreferred af labss labs = isComplete af labs && 
                            all (not . (inLab labs `subset` )) 
                                (map inLab (delete labs labss))

-- Definition 8 of Caminada, stable labelling
-- |Let 'labs' be a complete labelling, i.e. 'isComplete af labs', we say that 
-- labs is a stable labelling iff @undecLab(labs) == []@
isStable :: Eq arg => DungAF arg -> [Labelling arg] -> Labelling arg -> Bool
isStable af _labss labs = isComplete af labs &&
                          null (undecLab labs)
                            
-- Definition 8 of Caminada, semi-stable labelling
-- |Let 'labs' be a complete labelling, i.e. @isComplete af labs@, we say that 
-- labs is a semi-stable labelling iff @undecLab labs@ is minimal 
-- (w.r.t. set inclusion).
isSemiStable :: Ord arg => DungAF arg -> [Labelling arg] -> Labelling arg -> Bool
isSemiStable af labss labs = isComplete af labs && 
                             all (undecLab labs `subset`) 
                                 (map undecLab labss)

-- Definition 9 of Caminada
-- |Given an AF, a labelling labs and an illegally in argument a in the af, 
-- (i.e. @illegallyIn af a labs@ => True),
-- a transition step on a in labs consists of the following: 
-- 1. the label of a is changed from 'In' to 'Out'
-- 2. for every b in {a} \cup a+, if b is illegally out,
-- then change the label from b from 'Out' to 'Undecided'
transitionStep :: Eq arg => DungAF arg -> Labelling arg -> arg -> Labelling arg
transitionStep af labs a = 
 let labs' = (a, Out) : delete (a, In) labs -- Step 1
     bs    = a : aplus af a -- bs = every b in {a} \cup a+
     (newUndecs, rest) = partition (\ lab@(b, _l) ->
                                       b `elem` bs
                                    && illegallyOut af labs' lab)
                                  labs'
 in map (\ (x, _) -> (x, Undecided)) newUndecs
 ++ rest


-- Based on Definition 10 of Caminada
-- Instead of checking termination of a transition sequence
-- This function implements a check of termination for a specific transition
-- last . 
-- |Given an AF, a labelling, labs, is terminated iff labs does not contain any argument that is 
-- illegally in, i.e. @not (illegallyIn af lab arg)@ for all arg in labs.
terminatedTransition :: Eq arg => DungAF arg -> Labelling arg -> Bool
terminatedTransition af labs = not . or $ map (illegallyIn af labs) labs

-- Definition 11 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is superillegally 'In' iff a is labelled 'In',
-- and it is attacked by an argument that is legally 'In' or legally 'Undecided'.
superIllegallyIn :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
superIllegallyIn af labs (a, In) = 
  not . null $ 
    [lab | lab <- labAttackers af a labs, 
           legallyIn af labs lab || legallyUndec af labs lab]
superIllegallyIn _  _    _      = False

-- Based on the Algorithm of Caminada
-- Instead of using a search tree and keeping a list of potential semi-stable
-- labellings, we remove the checks. 
-- Note that this actually gives us an algorithm for computing at least the 
-- maximal and minimal complete labellings, allowing us to then filter out 
-- the grounded, preferred, stable or semi-stable labellings dependent on 
-- what should be maximal or minimal.
-- |Computes maximal and minimal complete labellings for a Dung argumentation 
-- framework. This is based on Caminada's algorithm for computing semi-stable
-- labellings, with all checks removed.
complete :: Ord arg => DungAF arg -> [Labelling arg]
complete af@(AF args _atk) =
 let allInArgs = allIn args
     complete' :: Eq arg => DungAF arg -> Labelling arg -> [Labelling arg]
     complete' af labs =
      case filter (superIllegallyIn af labs) labs of
            []          -> case filter (illegallyIn af labs) labs of
                             [] -> [labs]
                             ills -> concatMap (complete' af) $
                                       map (transitionStep af labs . fst) 
                                           ills
            ((a,_) : _) -> complete' af (transitionStep af labs a)
 in nub . map sort $ complete' af allInArgs
 
-- |Computes all preferred labellings for a Dung argumentation framework, by
-- taking the maximally in complete labellings.
preferred :: Ord arg => DungAF arg -> [Labelling arg]
preferred af =
 let completes = complete af 
 in filter (isPreferred af completes) completes

-- |Computes all stable labellings for a Dung argumentation framework, by
-- keeping only those labellings with no 'Undecided' labels.
stable :: Ord arg => DungAF arg -> [Labelling arg]
stable af =
 let completes = complete af 
 in filter (isStable af completes) completes

-- |Computes all semi-stable labellings for a Dung argumentation framework, by
-- taking the minimally undecided complete labellings.
semiStable :: Ord arg => DungAF arg -> [Labelling arg]
semiStable af =
 let completes = complete af 
 in filter (isSemiStable af completes) completes

-- |The complete extension of an argumentation framework is just the complete labelling, 
-- keeping only those arguments that were labelled 'In'.
completeExt :: Ord arg => DungAF arg -> [[arg]]
completeExt af = [[arg | (arg, In) <- c] | c <- complete af]

-- |The preferred extension of an argumentation framework is just the preferred labelling, 
-- keeping only those arguments that were labelled 'In'.
preferredExt :: Ord arg => DungAF arg -> [[arg]]
preferredExt af = [[arg | (arg, In) <- c] | c <- preferred af]

-- |The stable extension of an argumentation framework is just the stable labelling, 
-- keeping only those arguments that were labelled 'In'.
stableExt :: Ord arg => DungAF arg -> [[arg]]
stableExt af = [[arg | (arg, In) <- c] | c <- stable af]

-- |The semi-stable extension of an argumentation framework is just the semi-stable labelling, 
-- keeping only those arguments that were labelled 'In'.
semiStableExt :: Ord arg => DungAF arg -> [[arg]]
semiStableExt af = [[arg | (arg, In) <- c] | c <- semiStable af]