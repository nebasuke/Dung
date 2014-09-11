-- |This module implements a command-line interface to the implementation of 
-- Dung's argumentation frameworks. Dung + Haskell = Dungell
--
-- Code in this module partly taken from/inspired by Shinobu
-- See: http://zuttobenkyou.wordpress.com/2011/04/19/haskell-using-cmdargs-single-and-multi-mode/
-- and http://listx.github.com/
{-# LANGUAGE DeriveDataTypeable, RecordWildCards #-}
module Main
  (
    main
  )
 where
import Language.Dung.AF(groundedExt, preferredExt, stableExt, semiStableExt,
                        DungAF(..))
import Language.Dung.Input
import Language.Dung.Output

import System.Console.CmdArgs
import System.Environment (getArgs, withArgs)
import System.Exit
<<<<<<< HEAD
import Control.Monad (when, unless)

data MyOptions = MyOptions {
  cegartix    :: Bool,
  laxCegartix :: Bool,
  fileName    :: String,
  outputFile  :: String,
  grounded    :: Bool,
  preferred   :: Bool,
  stable      :: Bool,
  semiStable  :: Bool,
  all         :: Bool
=======
import Control.Monad (when)

data MyOptions = MyOptions {
  cegartix   :: Bool,
  fileName   :: String,
  outputFile :: String,
  grounded   :: Bool,
  preferred  :: Bool,
  stable     :: Bool,
  semiStable :: Bool,
  all        :: Bool
>>>>>>> f6bf86df65ed1e6185afe157c69c5660b8ef523b
 } deriving (Show, Data, Typeable)

myProgOpts :: MyOptions
myProgOpts = MyOptions
<<<<<<< HEAD
    { cegartix    = True  &= help "Output in strict CEGARTIX/PrefSat format (standard)" 
    , laxCegartix = False &= help "Output in lax CEGARTIX/PrefSat format (+parentheses)" 
    , fileName    = def   &= typFile &= help "Name of the file to be read"
    , outputFile  = def   &= typFile &= help "Name of the file to be written"
    , grounded    = False &= help "Output grounded extension for the AF"
    , preferred   = False &= help "Output preferred extensions for the AF"
    , stable      = False &= help "Output stable extensions for the AF"
    , semiStable  = False &= help "Output semi-stable extensions for the AF"
    , all         = False &= help "Output extensions of all implemented semantics for AF"
=======
    { cegartix   = True  &= help "Output in CEGARTIX/PrefSat format (standard)" 
    , fileName   = def   &= typFile &= help "Name of the file to be read"
    , outputFile = def   &= typFile &= help "Name of the file to be written"
    , grounded   = False &= help "Output grounded extension for the AF"
    , preferred  = False &= help "Output preferred extensions for the AF"
    , stable     = False &= help "Output stable extensions for the AF"
    , semiStable = False &= help "Output semi-stable extensions for the AF"
    , all        = False &= help "Output extensions of all implemented semantics for AF"
>>>>>>> f6bf86df65ed1e6185afe157c69c5660b8ef523b
    }
 
getOpts :: IO MyOptions
getOpts = cmdArgs $ myProgOpts
    -- &= verbosityArgs [explicit, name "Verbose", name "V"] []
    &= versionArg [explicit, name "version", name "v", summary _PROGRAM_INFO]
    &= summary (_PROGRAM_INFO ++ ", " ++ _COPYRIGHT)
    &= help _PROGRAM_ABOUT
    &= helpArg [explicit, name "help", name "h"]
    &= program _PROGRAM_NAME
 
_PROGRAM_NAME = "Dungell"
<<<<<<< HEAD
_PROGRAM_VERSION = "1.0.0.1"
=======
_PROGRAM_VERSION = "1.0"
>>>>>>> f6bf86df65ed1e6185afe157c69c5660b8ef523b
_PROGRAM_INFO = _PROGRAM_NAME ++ " version " ++ _PROGRAM_VERSION
_PROGRAM_ABOUT = "An implementation of Dung's AFs"
_COPYRIGHT = "(C) Bas van Gijzel 2014"


main :: IO ()
main = do 
        args <- getArgs
        opts <- (if null args then withArgs ["--help"] else id) getOpts
        optionHandler opts

-- |Check any malformed arguments/missing arguments. 
optionHandler :: MyOptions -> IO ()
<<<<<<< HEAD
optionHandler opts@MyOptions{..}  = do 
    when (null fileName) $ putStrLn "--fileName is blank!" >> exitWith (ExitFailure 1)
    input <- readFile fileName
    let opts' = opts {cegartix = not laxCegartix}
    af <- case parseAF input of 
           Left err -> putStrLn "Parsing error: " >> print err >> exitWith (ExitFailure 1)
           Right af -> return af
    let opts'' = if all 
         then 
           opts' {grounded = True, preferred = True, stable = True, semiStable = True} 
         else 
           opts'
    exec opts'' af
=======
optionHandler opts@MyOptions{..}  = do
    when (null fileName) $ putStrLn "--fileName is blank!" >> exitWith (ExitFailure 1)
    input <- readFile fileName
    af <- case (parseAF input) of 
           Left err -> putStrLn "Parsing error: " >> print err >> exitWith (ExitFailure 1)
           Right af -> return af
    let opts' = if all 
         then 
           opts {grounded = True, preferred = True, stable = True, semiStable = True} 
         else 
           opts
    exec opts' af
>>>>>>> f6bf86df65ed1e6185afe157c69c5660b8ef523b

-- |Execute supplied options
exec :: (Show arg, Eq arg, Ord arg) => MyOptions -> DungAF arg -> IO ()
exec opts@MyOptions{..} af = do
    print af
    when grounded   $ putStr "grounded: "    >> print (groundedExt af)
    when preferred  $ putStr "preferred: "   >> print (preferredExt af)
    when stable     $ putStr "stable: "      >> print (stableExt af)
    when semiStable $ putStr "semi-stable: " >> print (semiStableExt af)
<<<<<<< HEAD
    unless (null outputFile)
      $ if cegartix 
          then writeFile outputFile (toStrictCegartix af) >> putStrLn "File outputted."
          else writeFile outputFile (toCegartix af) >> putStrLn "File outputted."
=======
    when (not . null $ outputFile) 
      $ writeFile outputFile (toCegartix af) >> putStrLn "File outputted."
>>>>>>> f6bf86df65ed1e6185afe157c69c5660b8ef523b
