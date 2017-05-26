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
import Control.Monad.Error
import Control.Monad


readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err  -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
     args <- getArgs
     evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
     putStrLn $ extractValue $ trapError evaled
