module Main 
    (
        main,
    )
    where

import System.Environment
import Scheme.Parser


main :: IO ()
main = do 
    (expr:_) <- getArgs
    putStrLn (readExpr expr)