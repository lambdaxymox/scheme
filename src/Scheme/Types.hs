module Scheme.Types
    (
        Env(..),
        LispVal(..),
        LispError(..),
        ThrowsError,
        IOThrowsError,
        trapError,
        extractValue,
    )
    where

import Control.Monad
import Control.Monad.Except
import Data.IORef
import System.IO
import Text.ParserCombinators.Parsec
import Data.Array


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | Character Char
             | Vector (Array Int LispVal)
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String), body :: [LispVal], closure :: Env }
             | IOFunc ([LispVal] -> IOThrowsError LispVal)
             | Port Handle


instance Eq LispVal where
    Atom st1 == Atom st2 = st1 == st2
    List l1 == List l2 = l1 == l2
    DottedList l1 v1 == DottedList l2 v2 = v1 == v2 
    Number num1 == Number num2 = num1 == num2
    String st1 == String st2 = st1 == st2
    Bool b1 == Bool b2 = b1 == b2
    Character c1 == Character c2 = c1 == c2
    Vector v1 == Vector v2 = v1 == v2
    _ == _ = False

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
    show (Character ch) = "\'" ++ show ch ++ "\'"
    show (Vector contents) = "#(" ++ show contents ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show (Func {params = args, vararg = varargs, body = body, closure = env}) =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"
    show (Port _) = "<IO port>"
    show (IOFunc _) = "<IO primitive>"


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


{-
instance Error LispError where
     noMsg = Default "An error has occurred"
     strMsg = Default
-}

type ThrowsError = Either LispError

type IOThrowsError = ExceptT LispError IO

type Env = IORef [(String, IORef LispVal)]

unwordsList :: [LispVal] -> String
unwordsList = unwords . map show

trapError :: (MonadError a m, Show a) => m [Char] -> m [Char]
trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val
