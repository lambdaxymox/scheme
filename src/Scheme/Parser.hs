module Scheme.Parser
    (
        readExpr,
    )
    where

import Text.ParserCombinators.Parsec hiding (spaces)
import System.Environment
import Control.Monad


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

spaces :: Parser ()
spaces = skipMany1 space

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

parseString :: Parser LispVal
parseString = do
    _ <- char '"'
    x <- many $ escapedChar <|> nonEscapedChar
    _ <- char '"'
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
parseNumber = liftM (Number . read) $ many1 digit
