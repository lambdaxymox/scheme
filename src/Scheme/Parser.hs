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


parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseNumber 
         <|> parseString
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x


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
