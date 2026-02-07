-- |This module implements a command-line interface to the implementation of
-- Dung's argumentation frameworks. Dung + Haskell = Dungell
{-# LANGUAGE RecordWildCards #-}
module Main
  ( main
  ) where

import Language.Dung.AF (groundedExt, preferredExt, stableExt, semiStableExt,
                          DungAF(..))
import Language.Dung.Input
import Language.Dung.Output

import Options.Applicative
import System.Exit
import Control.Monad (when)

data Semantics
  = Grounded
  | Preferred
  | Stable
  | SemiStable
  deriving (Eq, Show)

data Options = Options
  { optLaxCegartix :: Bool
  , optFileName    :: FilePath
  , optOutputFile  :: Maybe FilePath
  , optSemantics   :: [Semantics]
  } deriving (Show)

semanticsFlags :: Parser [Semantics]
semanticsFlags = combineFlags
  <$> switch (long "grounded"    <> help "Output grounded extension for the AF")
  <*> switch (long "preferred"   <> help "Output preferred extensions for the AF")
  <*> switch (long "stable"      <> help "Output stable extensions for the AF")
  <*> switch (long "semi-stable" <> help "Output semi-stable extensions for the AF")
  <*> switch (long "all"         <> help "Output extensions of all implemented semantics for AF")
  where
    combineFlags gr pr st ss allSem
      | allSem    = [Grounded, Preferred, Stable, SemiStable]
      | otherwise = concat
          [ [Grounded   | gr]
          , [Preferred  | pr]
          , [Stable     | st]
          , [SemiStable | ss]
          ]

optionsParser :: Parser Options
optionsParser = Options
  <$> switch
        (  long "lax-cegartix"
        <> help "Output in lax CEGARTIX/PrefSat format (+parentheses)"
        )
  <*> strOption
        (  long "filename"
        <> metavar "FILE"
        <> help "Name of the file to be read"
        )
  <*> optional (strOption
        (  long "outputfile"
        <> metavar "FILE"
        <> help "Name of the file to be written"
        ))
  <*> semanticsFlags

opts :: ParserInfo Options
opts = info (optionsParser <**> helper)
  (  fullDesc
  <> progDesc "An implementation of Dung's AFs"
  <> header "dungell - Dung + Haskell argumentation framework tool"
  )

main :: IO ()
main = do
  Options{..} <- execParser opts
  input <- readFile optFileName
  af <- case parseAF input of
    Left err -> do
      putStrLn "Parsing error: "
      print err
      exitWith (ExitFailure 1)
    Right af -> return af
  exec optLaxCegartix optOutputFile optSemantics af

exec :: (Show arg, Ord arg)
     => Bool -> Maybe FilePath -> [Semantics] -> DungAF arg -> IO ()
exec laxCegartix outputFile semantics af = do
  print af
  when (Grounded   `elem` semantics) $ putStr "grounded: "    >> print (groundedExt af)
  when (Preferred  `elem` semantics) $ putStr "preferred: "   >> print (preferredExt af)
  when (Stable     `elem` semantics) $ putStr "stable: "      >> print (stableExt af)
  when (SemiStable `elem` semantics) $ putStr "semi-stable: " >> print (semiStableExt af)
  case outputFile of
    Nothing -> return ()
    Just fp ->
      if not laxCegartix
        then writeFile fp (toStrictCegartix af) >> putStrLn "File outputted."
        else writeFile fp (toCegartix af) >> putStrLn "File outputted."
