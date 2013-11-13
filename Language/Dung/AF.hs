-- | This module implements Dung's argumentation frameworks. 
module Language.Dung.AF 
 (
    -- * Basic definitions  
   DungAF(..), 
   setAttacks, conflictFree, acceptable, f, admissible, unattacked, attacked, 
   -- * Grounded semantics through fixpoints and labelling
   groundedF, Status(..), grounded, groundedExt)
 where
import Data.List (intersect, (\\))


-- |An abstract argumentation framework is a set of arguments 
-- (represented as a list) and an attack relation on these arguments. 
data DungAF arg = AF [arg] [(arg, arg)]
  deriving (Eq, Show)


-- |Given an argumentation framework, determines whether args 
-- (subset of the arguments in the AF), attacks an argument arg (in the AF).
setAttacks :: Eq arg => DungAF arg -> [arg] -> arg -> Bool
setAttacks (AF _ def) args arg 
  = or [b == arg | (a, b) <- def, a `elem` args] 

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

-- |Given an argumentation framework, determines whether an  
-- argument is acceptable with respect to 'args' (subset of the arguments in the AF). 
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
-- if 'args' is 'conflictFree' and each argument in args is acceptable with respect to args.
-- admissible af args = conflictFree af args && and [acceptable af arg args | arg <- args]

-- |Given a characteristic function f, computes the grounded extension
-- by iterating on the empty set (list) until it reaches a fixpoint.
groundedF :: Eq arg => ([arg] -> [arg]) -> [arg]
groundedF f = groundedF' f []
  where  groundedF' f args 
           | f args == args  = args
           | otherwise       = groundedF' f (f args)


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

  
-- |Labelling of arguments.
data Status = In | Out | Undecided
  deriving (Eq, Show)

-- |Computes the grounded labelling for a Dung argumentation framework,
-- returning a list of arguments with statuses.
-- 
-- Based on section 4.1 of Proof Theories and Algorithms for Abstract Argumentation Frameworks
-- by Modgil and Caminada
grounded :: Eq arg => DungAF arg -> [(arg, Status)]
grounded af@(AF args _) = grounded' [] [] args af
 where 
 grounded' :: Eq a => [a] -> [a] -> 
              [a] -> DungAF a -> [(a, Status)]
 grounded' ins outs [] _   
  =    map (\ x -> (x, In)) ins 
    ++ map (\ x -> (x, Out)) outs
 grounded' ins outs args af  = 
   let newIns   = filter (unattacked outs af)  args
       newOuts  = filter (attacked ins af)     args
   in if null (newIns ++ newOuts) 
      then  map (\ x -> (x, In)) ins
        ++  map (\ x -> (x, Out)) outs 
        ++  map (\ x -> (x, Undecided)) args
      else grounded' (ins ++ newIns) 
                     (outs ++ newOuts) 
                     (args \\ (newIns ++ newOuts)) 
                     af

-- |The grounded extension of an argumentation framework is just the grounded labelling, 
-- keeping only those arguments that were labelled 'In'.
groundedExt :: Eq arg => DungAF arg -> [arg]
groundedExt af = [arg | (arg, In) <- grounded af] 






