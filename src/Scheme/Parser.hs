module Scheme.Parser
    (
        parseExpr,
    )
    where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric
import Scheme.Env
import Scheme.Types
import Data.Array


parseExpr :: Parser LispVal
parseExpr = parseSharped
        <|> parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> parseChar
        <|> do char '('
               x <- try parseList <|> parseDottedList
               char ')'
               return x
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> try parseVector


escapedChars :: String
escapedChars = "\"\n\r\t\\\'"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

skipSpaces :: Parser ()
skipSpaces = skipMany1 space

nonEscapedChar :: Parser Char
nonEscapedChar = noneOf escapedChars

escapedChar :: Parser Char
escapedChar = do 
    char '\\'
    escapeChar <- anyChar
    return $ 
        case escapeChar of
            'n' -> '\n'
            'r' -> '\r'
            't' -> '\t'
            _  -> escapeChar

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChar <|> noneOf "\"")
    char '"'
    return $ String x

parseAtom :: Parser LispVal
parseAtom = do
    first <- letter <|> symbol
    rest  <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of 
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

parseNumber :: Parser LispVal
parseNumber = do
    prefix <- numberPrefix <|> string ""
    case prefix of
        ""   -> Number . read <$> decDigits
        "#b" -> Number . fst . head . readBin <$> binDigits
        "#o" -> Number . fst . head . readOct <$> octDigits
        "#d" -> Number . read <$> decDigits
        "#x" -> Number . fst . head . readHex <$> hexDigits           

hexDigits :: Parser String
hexDigits = many1 hexDigit

octDigits :: Parser String
octDigits = many1 octDigit

decDigits :: Parser String
decDigits = many1 digit

binDigit :: Parser Char
binDigit = oneOf "01"

binDigits :: Parser String
binDigits = many1 binDigit

truthValue :: Parser String
truthValue = do
    ch1 <- char '#'
    ch2 <- oneOf "tf"
    return [ch1,ch2]

numberPrefix :: Parser String
numberPrefix = do
    ch1 <- char '#'
    ch2 <- oneOf "bodx"
    return [ch1, ch2]

vectorPrefix :: Parser String
vectorPrefix = string "#("

charPrefix :: Parser String
charPrefix = string "#\\"

readBin :: ReadS Integer
readBin = readInt 2 isBinaryDigit binConvert
    where
        isBinaryDigit :: Char -> Bool
        isBinaryDigit ch = ch `elem` "01"

        binConvert :: Char -> Int
        binConvert '0' = 0
        binConvert '1' = 1
        binConvert  _  = error "readBin failed."

parseList :: Parser LispVal
parseList = List <$> sepBy parseExpr skipSpaces

parseDottedList :: Parser LispVal
parseDottedList = do
    lhead <- endBy parseExpr skipSpaces
    ltail <- char '.' >> skipSpaces >> parseExpr
    return $ DottedList lhead ltail

parseQuoted :: Parser LispVal
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

parseSharped :: Parser LispVal
parseSharped = do
    st <- lookAhead $  try truthValue 
                   <|> try numberPrefix 
                   <|> try vectorPrefix
                   <|> try charPrefix
    case st of
        "#t"  -> parseAtom
        "#f"  -> parseAtom
        "#b"  -> parseNumber
        "#o"  -> parseNumber
        "#d"  -> parseNumber
        "#x"  -> parseNumber
        "#("  -> parseVector
        "#\\" -> parseChar

parseChar :: Parser LispVal
parseChar = do 
    try $ string "#\\"        
    value <- try (string "newline" <|> string "space")
             <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
    return $ Character $ case value of
       "space"   -> ' '
       "newline" -> '\n'
       _         -> head value

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do
    char '`'
    x <- parseExpr
    return $ List [Atom "quasiquote", x]

parseUnQuote :: Parser LispVal
parseUnQuote = do
    char ','
    x <- parseExpr
    return $ List [Atom "unquote", x]

parseVector :: Parser LispVal
parseVector = do
    string "#("
    vec <- parseVector'
    char ')'
    return vec

parseVector' :: Parser LispVal
parseVector' = do 
    arrayValues <- sepBy parseExpr skipSpaces
    return $ Vector (listArray (0, length arrayValues - 1) arrayValues)
