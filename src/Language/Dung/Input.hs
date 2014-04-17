-- | This is the input module accompanying the implementation of Dung's 
-- argumentation frameworks. It defines a simple parser for an argumentation framework
-- that assumes the input file is in CEGARTIX/PrefSat-like format.
--
-- Files are assumed to have one argument or attack on each line, ending
-- in a dot. (Our parser is slightly more relaxed than this and doesn't care about whitespace.)
--
-- @att(a1,a2).@ or @arg(a1).@
--
-- Argument names are assumed to consist only of letters and numbers.
-- Arguments used in attacks should be declared separately as well. 

module Language.Dung.Input
  (
   -- * Parsing functions
   parseAF, pAF
   )
 where
import Language.Dung.AF
import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Char (char, string)
import qualified Text.Parsec.Token as P
import Text.Parsec.Language(emptyDef)
import Text.Parsec.Error(errorMessages, messageString)
import Data.Either (partitionEithers)

lexer :: P.TokenParser ()
lexer = P.makeTokenParser emptyDef

whiteSpace :: Parser ()
whiteSpace = P.whiteSpace lexer

-- |An argument name consists of one or more letters and digits.
argName :: Parser String
argName = many1 alphaNum <?> "Argument name"

-- |A complete argument consists of @arg(argName).@
pArgument :: Parser String
pArgument = do 
               string "arg("
               arg <- argName
               string ")."
               whiteSpace
               return arg

-- |A complete attack consists of @atk(argName,argName).@
-- or @att(argName,argName).@.
pAttack :: Parser (String, String)
pAttack = do 
             string "at"
             string "t(" <|> string "k("
             arg1 <- argName
             char ','
             whiteSpace
             arg2 <- argName
             string ")."
             return (arg1, arg2)

-- |Parses one attack or argument and returns the result
-- in the 'Either' data type.
pArgOrAttack :: Parser (Either String (String, String))
pArgOrAttack = (try (do arg <- pArgument 
                        whiteSpace
                        return $ Left arg))
               <|> 
               do atk <- pAttack
                  whiteSpace
                  return $ Right atk

-- |An AF is parsed by parsing at least one argument or attack,
-- followed by an end of file token.
pAF :: Parser (DungAF String)
pAF = do  
          ps <- many1 pArgOrAttack
          eof
          let (args, atks) = partitionEithers ps
          return $ AF args atks

-- |Parses a 'String' containing multiple arguments/attacks. 
-- If parsing fails, it propagates the parse error.
parseAF :: String -> Either ParseError (DungAF String)
parseAF input = parse pAF "" input