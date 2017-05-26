module Main 
    (
        main,
    )
    where

import System.Environment
import Scheme.Parser
import Scheme.Eval
import Scheme.LispVal
import Text.ParserCombinators.Parsec


readExpr :: String -> LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> String $ "No match: " ++ show err
    Right val -> val

main :: IO ()
main = getArgs >>= print . eval . readExpr . head