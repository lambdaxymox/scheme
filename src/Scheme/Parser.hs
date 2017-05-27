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
parseExpr = parseAtom
        <|> parseString
        <|> parseNumber
        <|> parseQuoted
        <|> parseChar
        <|> do char '('
               x <- (try parseList) <|> parseDottedList
               char ')'
               return x
        <|> parseQuasiQuoted
        <|> parseUnQuote
        <|> try (do string "#("
                    x <- parseVector
                    char ')'
                    return x)


escapedChars :: String
escapedChars = "\"\n\r\t\\\'"

symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

skipSpaces :: Parser ()
skipSpaces = skipMany1 space

nonEscapedChar :: Parser Char
nonEscapedChar = noneOf escapedChars

escapedChar :: Parser Char
escapedChar = oneOf escapedChars

parseString :: Parser LispVal
parseString = do
    char '"'
    x <- many (escapedChar <|> nonEscapedChar)
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
        ""   -> liftM (Number . read) decDigits
        "#b" -> liftM (Number . fst . head . readBin) $ binDigits
        "#o" -> liftM (Number . fst . head . readOct) $ octDigits
        "#d" -> liftM (Number . read) decDigits
        "#x" -> liftM (Number . fst . head . readHex) $ hexDigits
            

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

numberPrefix :: Parser String
numberPrefix = do -- char '#' >>= \ch1 -> oneOf "bodx" >>= \ch2 -> return [ch1,ch2]
    ch1 <- char '#'
    ch2 <- oneOf "bodx"
    return [ch1, ch2]

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
parseList = liftM List $ sepBy parseExpr skipSpaces

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

parseChar :: Parser LispVal
parseChar = do 
    try $ string "#\\"        
    value <- try (string "newline" <|> string "space")
             <|> do {x <- anyChar; notFollowedBy alphaNum; return [x]}
    return $ Character $ case value of
       "space"   -> ' '
       "newline" -> '\n'
       otherwise -> (value !! 0)

{- Exercise 3.4.1
   Add support for the backquote syntactic sugar: the Scheme standard details what
   it should expand into (quasiquote/unquote). -}

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

{- Exercise 3.4.2
   Add support for vectors. The Haskell representation is up to you: GHC does have
   an Array data type, but it can be difficult to use. Strictly speaking, a vector 
   should have constant-time indexing and updating, but destructive update in a 
   purely functional language is difficult. You may have a better idea how to do
   this after the section on set!, later in this tutorial. -}

parseVector :: Parser LispVal
parseVector = do 
            arrayValues <- sepBy parseExpr skipSpaces
            return $ Vector (listArray (0,(length arrayValues - 1)) arrayValues)
