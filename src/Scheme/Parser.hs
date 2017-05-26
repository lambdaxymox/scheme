module Scheme.Parser
    (
        readExpr,
    )
    where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad
import Numeric


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

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

readExpr :: String -> String
readExpr input = case parse parseExpr "lisp" input of
    Left err -> "No match: " ++ show err
    Right _  -> "Found value"

parseExpr :: Parser LispVal
parseExpr =  parseAtom
         <|> parseString
         <|> parseNumber
         <|> parseQuoted
         <|> do char '('
                x <- try parseList <|> parseDottedList
                char ')'
                return x

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
    digits <- many1 digit
    return $ case prefix of
        ""   -> Number . read $ digits
        "#b" -> Number . fst . head . readBin $ digits
        "#o" -> Number . fst . head . readOct $ digits
        "#d" -> Number . read $ digits
        "#x" -> Number . fst . head . readHex $ digits

numberPrefix :: Parser String
numberPrefix = char '#' >>= \ch1 -> oneOf "bodx" >>= \ch2 -> return [ch1,ch2]

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

