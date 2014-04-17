-- | This is the output module accompanying the implementation of Dung's 
-- argumentation frameworks. It allows an implemented argumentation framework
-- to be outputted to files in a standard format.
--
-- This module currently contains one output format, readable by both
-- CEGARTIX and PrefSat. 
module Language.Dung.Output 
  (
   -- * CEGARTIX/PrefSat output
   argToCegartix, atkToCegartix, toCegartix
   )
 where
import Language.Dung.AF

-- |Converts an argument to a CEGARTIX 'String'. Quotes are removed.
argToCegartix :: Show arg => arg -> String
argToCegartix arg = remQuote $ "arg(" ++ show arg ++ ").\n" 

-- |Converts an attack to a CEGARTIX 'String'. Quotes are removed.
atkToCegartix :: Show arg => (arg, arg) -> String
atkToCegartix (a,b) = remQuote $ "att(" ++ show a ++ "," ++ show b ++ ").\n"


-- |Outputs an argumentation frameworks in CEGARTIX/PrefSat format.
toCegartix :: Show arg => DungAF arg -> String
toCegartix (AF args att) = 
    concatMap argToCegartix args
 ++ concatMap atkToCegartix att

-- toCegartix :: Show arg => DungAF arg -> IO ()
-- toCegartix (AF args att) = do 
  -- mapM_ (putStr . argToCegartix) args
  -- mapM_ (putStr . atkToCegartix) att

-- |Remove all quotes from a 'String'.
remQuote :: String -> String
remQuote = filter (/= '"')