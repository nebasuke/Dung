-- | This is the output module accompanying the implementation of Dung's 
-- argumentation frameworks. It allows an implemented argumentation framework
-- to be outputted to files in a standard format.
--
-- This module currently contains two output format. The strict version is readable 
-- by both CEGARTIX and PrefSat. The lax version keeps more of the original formatting.
module Language.Dung.Output 
  (
   -- * CEGARTIX/PrefSat output
   argToCegartix, atkToCegartix, toCegartix,
   argToStrictCegartix, atkToStrictCegartix, toStrictCegartix
   )
 where
import Language.Dung.AF


-- |Converts an argument to a CEGARTIX 'String'. All argument names are made
-- into string literals removing extra quotes. Additionally all parentheses are removed.
argToStrictCegartix :: Show arg => arg -> String
argToStrictCegartix arg = "arg(" ++ (show . remParens . remQuote . show) arg ++ ").\n" 

-- |Converts an attack to a CEGARTIX 'String'. All argument names are made
-- into string literals removing extra quotes. Additionally all parentheses are removed.
atkToStrictCegartix :: Show arg => (arg, arg) -> String
atkToStrictCegartix (a,b) = "att(" ++ (show . remParens . remQuote . show) a ++ "," ++ (show . remQuote. show) b ++ ").\n"

-- |Converts an argument to a CEGARTIX 'String'. All argument names are made
-- into string literals removing extra quotes.
argToCegartix :: Show arg => arg -> String
argToCegartix arg = "arg(" ++ (show . remQuote . show) arg ++ ").\n" 

-- |Converts an attack to a CEGARTIX 'String'. All argument names are made
-- into string literals removing extra quotes.
atkToCegartix :: Show arg => (arg, arg) -> String
atkToCegartix (a,b) = "att(" ++ (show . remQuote . show) a ++ "," ++ (show . remQuote. show) b ++ ").\n"

-- |Outputs an argumentation frameworks in CEGARTIX/PrefSat format.
toCegartix :: Show arg => DungAF arg -> String
toCegartix (AF args att) = 
    concatMap argToCegartix args
 ++ concatMap atkToCegartix att

-- |Outputs an argumentation frameworks in strict CEGARTIX/PrefSat format.
toStrictCegartix :: Show arg => DungAF arg -> String
toStrictCegartix (AF args att) = 
    concatMap argToStrictCegartix args
 ++ concatMap atkToStrictCegartix att

-- toCegartix :: Show arg => DungAF arg -> IO ()
-- toCegartix (AF args att) = do 
  -- mapM_ (putStr . argToCegartix) args
  -- mapM_ (putStr . atkToCegartix) att

-- |Remove all quotes from a 'String'.
remQuote :: String -> String
remQuote = filter (/= '"')

-- |Remove all parentheses from a 'String'.
remParens :: String -> String
remParens = filter (\ x -> x /= '(' && x /= ')')