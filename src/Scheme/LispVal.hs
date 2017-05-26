module Scheme.LispVal
    (
        LispVal(..),
        LispError(..),
        ThrowsError,
        extractValue,
        trapError,
    )
    where

import Control.Monad.Except
import Text.ParserCombinators.Parsec


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"


data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

instance Show LispError where
    show (UnboundVar message varname)  = message ++ ": " ++ varname
    show (BadSpecialForm message form) = message ++ ": " ++ show form
    show (NotFunction message func)    = message ++ ": " ++ show func
    show (NumArgs expected found)      = "Expected " ++ show expected 
                                       ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expected found) = "Invalid type: expected " ++ expected
                                       ++ ", found " ++ show found
    show (Parser parseErr)             = "Parse error at " ++ show parseErr


unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

{-
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default
-}

type ThrowsError = Either LispError


trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
