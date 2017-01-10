module SimpleKeyValue (Parser, parseNumber, parseFile, File(..), Value(..), Atom(..)) where

import Control.Applicative (Applicative(..), (<$), (<$>), Alternative(..))
import Control.Monad (ap)
import Data.Map (Map)
import Data.Set (Set)
import Numeric
import Text.Parsec hiding ((<|>), many)
import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Text.Parsec as P

infixl 4 <$$>
m <$$> f = f <$> m

type Parser = Parsec String ()

data File = File { keyValues :: Map String Value
                 , unnamed   :: [Value]
                 }
  deriving Show

data Value
  = Atom Atom
  | List [Atom]
  deriving (Eq, Show)

data Atom
  = String String
  | Number Integer
  deriving (Eq, Show)


------------------------------------------------------------
-- Lexemes -------------------------------------------------
------------------------------------------------------------

parseIdent :: Parser String
parseIdent = lexeme ((:) <$> letter <*> many (alphaNum <|> char '_')
                     <?> "identifier")

parseString :: Parser String
parseString = lexeme (parseString' '"' <|> parseString' '\'')

parseNumber :: Parser Integer
parseNumber = lexeme $
              char '0' *> (char 'x' *> many1 hexDigit <$$> readHex'
                           <|>         many1 octDigit <$$> readOct'
                           <|>         return 0)
          <|>                          many1 digit    <$$> read

escapeCode :: Parser Char
escapeCode
  =           char '\\'
  <|>         char '\''
  <|>         char '"'
  <|> '\n' <$ char 'n'
  <|> '\t' <$ char 't'
  <|> '\a' <$ char 'a'
  <|> '\b' <$ char 'b'
  <|> '\f' <$ char 'f'
  <|> '\n' <$ char 'n'
  <|> '\r' <$ char 'r'
  <|> '\t' <$ char 't'
  <|> '\v' <$ char 'v'
  <|> char 'x' *> hexNumber
  <|> octNumber
  <|> pure '\\'
  where
  hexNumber = count  2 hexDigit <$$> readHex' <$$> toEnum
  octNumber = many1N 3 octDigit <$$> readOct' <$$> toEnum

escapedChar :: Char -> Parser Char
escapedChar c = newline   *> unexpected "new-line"
            <|> char '\\' *> escapeCode
            <|> noneOf [c]
            <?> "string character"

parseString' :: Char -> Parser String
parseString' c = between (char c) (char c <?> "end of string")
                         (many (escapedChar c))

------------------------------------------------------------
-- Configuration File Parser -------------------------------
------------------------------------------------------------

parseFile :: Parser File
parseFile = do
  spaces
  skipMany parseEndStmt
  xs <- parseSetting `sepEndBy` skipMany1 parseEndStmt
  let kvs    = [ (k,v) | (Just k, v) <- xs ]
      others = [ v     | (Nothing, v) <- xs ]
  checkForDuplicates (map fst kvs)
  eof
  return (File (Map.fromList kvs) others)

checkForDuplicates :: [String] -> Parser ()
checkForDuplicates xs =
  case findDuplicate xs of
    Nothing -> return ()
    Just x  -> fail ("Variable reassigned: " ++ show x)

findDuplicate :: Ord a => [a] -> Maybe a
findDuplicate = aux Set.empty
  where
  aux s [] = Nothing
  aux s (x : xs)
    | x `Set.member` s = Just x
    | otherwise        = aux (Set.insert x s) xs

parseSetting :: Parser (Maybe String, Value)
parseSetting = do
  mbi <- optionMaybe (parseIdent <* symbol '=')
  val <- parseValue
  return (mbi, val)

parseValue :: Parser Value
parseValue = Atom <$> parseAtom
         <|> List <$> parseList
         <?> "value"

parseAtom :: Parser Atom
parseAtom = String <$> parseString
        <|> Number <$> parseNumber

parseList :: Parser [Atom]
parseList = between (symbolnl '[') (symbol ']')
                    (nltok parseAtom `sepEndBy` symbolnl ',')


parseEndOfLine :: Parser ()
parseEndOfLine = optional parseComment <* lexeme newline

parseEndStmt :: Parser ()
parseEndStmt = parseEndOfLine
           <|> () <$ symbol ';'

parseComment :: Parser ()
parseComment = char '#' *> skipMany (noneOf "\n") <?> "comment"

------------------------------------------------------------
-- Token combinators ---------------------------------------
------------------------------------------------------------

lexeme :: Parser a -> Parser a
lexeme m = m <* skipMany (oneOf " \t")

nltok :: Parser a -> Parser a
nltok m = m <* skipMany parseEndOfLine

symbol :: Char -> Parser Char
symbol = lexeme . char

symbolnl :: Char -> Parser Char
symbolnl = nltok . symbol

------------------------------------------------------------
-- Numeric Utilities ---------------------------------------
------------------------------------------------------------

readHex', readOct' :: (Eq a, Num a) => String -> a
readHex' str = case readHex str of [(n,"")] -> n
readOct' str = case readOct str of [(n,"")] -> n

------------------------------------------------------------
-- Parser Utilities ----------------------------------------
------------------------------------------------------------

-- | 'manyN' parses 0 to 'n' (inclusive) instances of 'p'.
manyN, many1N :: Int -> Parser a -> Parser [a]
manyN n p = option [] (many1N n p)

-- | 'many1N' parses 1 to 'n' (inclusive) instances of 'p'.
many1N 0 _ = empty
many1N n p = do
  x  <- p
  xs <- manyN (n-1) p
  return (x:xs)
