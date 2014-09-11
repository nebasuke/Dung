-- | This module implements Dung's argumentation frameworks. 
module Language.Dung.AF 
 (
    -- * Basic definitions  
   DungAF(..), 
   setAttacks, aplus, amin, argplus, argmin, 
   conflictFree, acceptable, f, admissible, 
   -- * Grounded, preferred, semi-stable and stable semantics through fixpoints
   groundedF,
   -- * Basic labelling definitions
   -- |The following functions are implementations of the 
   -- definitions in \"An algorithm for Computing Semi-Stable 
   -- Semantics\" in \"Symbolic and Quantitative Approaches to Reasoning with
   -- Uncertainty\", pages 222--234, Springer, 2007.
   Status(..), Labelling(..), 
   inLab, outLab, undecLab, 
   allIn, allOut, allUndec,
   unattacked, attacked, 
   labAttackers, illegallyIn, illegallyOut, illegallyUndec,
   legallyIn, legallyOut, legallyUndec,
   isAdmissible, isComplete, isPreferred, isStable, isSemiStable,
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
import Data.List (intersect, (\\), partition, delete, nub, sort)


-- |An abstract argumentation framework is a set of arguments 
-- (represented as a list) and an attack relation on these arguments. 
data DungAF arg = AF [arg] [(arg, arg)]
  deriving (Eq, Show)

-- |Given an argumentation framework, determines whether args 
-- (subset of the arguments in the AF), attacks an argument arg (in the AF).
setAttacks :: Eq arg => DungAF arg -> [arg] -> arg -> Bool
setAttacks (AF _ def) args arg 
  = or [b == arg | (a, b) <- def, a `elem` args] 

-- |Given an argumentation framework, determines the set of arguments
-- that are attacked by an argument (in the AF).
aplus :: Eq arg => DungAF arg -> arg -> [arg]
aplus (AF args atk) a = [b | (a', b) <- atk, a == a']

-- |Given an argumentation framework, determines the set of arguments
-- attacking an argument (in the AF).
amin :: Eq arg => DungAF arg -> arg -> [arg]
amin (AF args atk) a = [b | (b, a') <- atk, a == a']

-- |Given an argumentation framework, determines the set of arguments
-- that are attacked by the given subset of arguments (in the AF).
argplus :: Eq arg => DungAF arg -> [arg] -> [arg]
argplus af = nub . concatMap (aplus af)

-- |Given an argumentation framework, determines the set of arguments
-- that attack a given subset of arguments (in the AF).
argmin :: Eq arg => DungAF arg -> [arg] -> [arg]
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
subset :: Eq a => [a] -> [a] -> Bool
xs `subset` ys = null (xs \\ ys)

-- |Given an argumentation framework, determines whether 
-- the set of arguments 'args' (subset of the arguments in the AF) is admissible,
-- i.e. if 'args' is 'conflictFree' and args is a subset of @f af args@
admissible :: Eq arg =>  DungAF arg -> [arg] -> Bool
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
groundedF f = groundedF' f []
  where  groundedF' f args 
           | f args == args  = args
           | otherwise       = groundedF' f (f args)



-------------------------------------------------------------------------------
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
unattacked :: Eq arg => [arg] -> 
              DungAF arg -> arg -> Bool
unattacked outs (AF _ def) arg = 
  let attackers = [a | (a, b) <- def, arg == b]
  in null (attackers \\ outs)

-- |Given a list of arguments that are 'In' in an argumentation framework af, 
-- an argument 'arg' is attacked if there exists an attacker that is 'In'.
attacked :: Eq arg => [arg] -> 
            DungAF arg -> arg -> Bool
attacked ins (AF _ def) arg = 
  let attackers = [a | (a, b) <- def, arg == b]
  in not (null (attackers `intersect` ins))


-- |Computes the grounded labelling for a Dung argumentation framework,
-- returning a (unique) list of arguments with statuses.
-- 
-- Based on section 4.1 of Proof Theories and Algorithms for Abstract Argumentation Frameworks
-- by Modgil and Caminada.
grounded :: Eq arg => DungAF arg -> Labelling arg
grounded af@(AF args _) = grounded' [] [] args af
 where 
 grounded' :: Eq a => [a] -> [a] -> 
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
groundedExt :: Eq arg => DungAF arg -> [arg]
groundedExt af = [arg | (arg, In) <- grounded af] 

-- |Given an argumentation framework, determines the list of attackers of an argument, 
-- from a given labelling, returning the labelled attackers. 
labAttackers :: Eq arg => DungAF arg -> arg -> Labelling arg -> Labelling arg
labAttackers (AF args atk) a labs = [lab | lab@(b, _) <- labs, (b, a) `elem` atk]

-- Definition 5.1 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is illegally 'In' iff a is labelled 'In',
-- but not all its attackers are labelled 'Out'.
illegallyIn :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
illegallyIn af labs (a, In) = not . null $ [lab | lab@(_, l) <- labAttackers af a labs, l /= Out]
illegallyIn _  _     _      = False

-- Definition 5.2 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is illegally 'Out' iff a is labelled 'Out'
-- but does not have an attacker labelled 'In'.
illegallyOut :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
illegallyOut af labs (a, Out) = null [lab | lab@(_, In) <- labAttackers af a labs]
illegallyOut _  _    _        = False

-- Definition 5.3 of Caminada
-- |Given an AF and 'Labelling',
-- an argument a (in the AF) is illegally 'Undecided' iff a is labelled 'Undecided' 
-- but either all its attackers are labelled 'Out' 
-- or it has an attacker that is labelled 'In'.
illegallyUndec :: Eq arg => DungAF arg -> Labelling arg -> (arg, Status) -> Bool
illegallyUndec af labs (a, Undecided) = and [l == Out | (_, l) <- labAttackers af a labs]
                                        || (not . null) [lab | lab@(_, In) <- labAttackers af a labs] 
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
                      [lab | lab@(a, In) <- labs, illegallyIn af labs lab] 
                   ++ [lab | lab@(a, Out) <- labs, illegallyOut af labs lab] 

-- Definition 7 of Caminada
-- |Given an AF, a complete labelling is a labelling without arguments
-- that are 'illegallyIn', without arguments that are 'illegallyOut' and 
-- without arguments that are 'illegallyUndec'.
isComplete ::  Eq arg => DungAF arg -> Labelling arg -> Bool
isComplete af labs = null $ 
                   [lab | lab@(a, In) <- labs, illegallyIn af labs lab] 
                ++ [lab | lab@(a, Out) <- labs, illegallyOut af labs lab] 
                ++ [lab | lab@(a, Undecided) <- labs, illegallyUndec af labs lab]


-- Definition 8 of Caminada, grounded labelling
-- |Let 'labs' be a complete labelling, i.e. @isComplete af labs@, we say that 
-- labs is a grounded labelling iff @inLab labs@ is minimal 
-- (w.r.t. set inclusion).
isGrounded :: Eq arg => DungAF arg -> [Labelling arg] -> Labelling arg -> Bool
isGrounded af labss labs = isComplete af labs && 
<<<<<<< HEAD
                           all (inLab labs `subset`) (map inLab labss)
=======
                           and (map (inLab labs `subset`) (map inLab labss))
>>>>>>> f6bf86df65ed1e6185afe157c69c5660b8ef523b

-- Definition 8 of Caminada, preferred labelling
-- |Let 'labs' be a complete labelling, i.e. @isComplete af labs@, we say that 
-- labs is a preferred labelling iff @inLab labs@ is maximal 
-- (w.r.t. set inclusion).
isPreferred :: Eq arg => DungAF arg -> [Labelling arg] -> Labelling arg -> Bool
isPreferred af labss labs = isComplete af labs && 
<<<<<<< HEAD
                            all (not . (inLab labs `subset` )) (map inLab (delete labs labss))
=======
                            and (map (not . (inLab labs `subset` )) (map inLab (delete labs labss)))
>>>>>>> f6bf86df65ed1e6185afe157c69c5660b8ef523b

-- Definition 8 of Caminada, stable labelling
-- |Let 'labs' be a complete labelling, i.e. 'isComplete af labs', we say that 
-- labs is a preferred labelling iff @undecLab(labs) == []@
isStable :: Eq arg => DungAF arg -> [Labelling arg] -> Labelling arg -> Bool
isStable af labss labs = isComplete af labs && 
                         null (undecLab labs)
                            
-- Definition 8 of Caminada, semi-stable labelling
-- |Let 'labs' be a complete labelling, i.e. @isComplete af labs@, we say that 
-- labs is a semi-stable labelling iff @undecLab labs@ is minimal 
-- (w.r.t. set inclusion).
isSemiStable :: Eq arg => DungAF arg -> [Labelling arg] -> Labelling arg -> Bool
isSemiStable af labss labs = isComplete af labs && 
<<<<<<< HEAD
                             all (undecLab labs `subset`) 
                                 (map undecLab labss)
=======
                             and (map (undecLab labs `subset`) 
                                      (map undecLab labss))
>>>>>>> f6bf86df65ed1e6185afe157c69c5660b8ef523b

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
     (newUndecs, rem) = partition (\ lab@(b, l) -> 
                                       b `elem` bs
                                    && illegallyOut af labs' lab)
                                  labs'
 in map (\ (a, _) -> (a, Undecided)) newUndecs
 ++ rem


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
-- Note that this actually gives us an algorithm for computing the complete 
-- labellings, allowing us to then filter out the grounded, preferred,
-- stable or semi-stable labellings dependent on what should be maximal or 
-- minimal
-- |Computes all complete labellings for a Dung argumentation framework. This
-- is based on Caminada's algorithm for computing semi-stable labellings, 
-- with all checks removed.
complete :: Ord arg => DungAF arg -> [Labelling arg]
complete af@(AF args atk) = 
 let allInArgs = allIn args
     complete' :: Eq arg => DungAF arg -> Labelling arg -> [Labelling arg]
     complete' af labs =
      case filter (superIllegallyIn af labs) labs of
            []          -> case filter (illegallyIn af labs) labs of
                             [] -> [labs]
                             ills -> concatMap (complete' af) $
<<<<<<< HEAD
                                       map (transitionStep af labs . fst) 
                                           ills
=======
                                       map (transitionStep af labs) 
                                           (map fst ills)
>>>>>>> f6bf86df65ed1e6185afe157c69c5660b8ef523b
            ((a,_) : _) -> complete' af (transitionStep af labs a)
 in nub . map sort $ complete' af allInArgs

-- |Computes all preferred labellings for a Dung argumentation framework, by
-- taking the maximally in complete labellings.
preferred :: Ord arg => DungAF arg -> [Labelling arg]
preferred af@(AF args atk) = 
 let completes = complete af 
 in filter (isPreferred af completes) completes

-- |Computes all stable labellings for a Dung argumentation framework, by
-- keeping only those labellings with no 'Undecided' labels.
stable :: Ord arg => DungAF arg -> [Labelling arg]
stable af@(AF args atk) = 
 let completes = complete af 
 in filter (isStable af completes) completes

-- |Computes all semi-stable labellings for a Dung argumentation framework, by
-- taking the minimally undecided complete labellings.
semiStable :: Ord arg => DungAF arg -> [Labelling arg]
semiStable af@(AF args atk) = 
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