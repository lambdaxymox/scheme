module Scheme.Env
    (
        Env(..),
        IOThrowsError,
        nullEnv,
        liftThrows,
        runIOThrows,
        isBound,
        getVar,
        setVar,
        defineVar,
        bindVars,
        LispVal(..),
        LispError(..),
        ThrowsError,
        extractValue,
        trapError,
    )
    where

import Data.IORef
import System.IO
import Control.Monad
import Control.Monad.Except
import Text.ParserCombinators.Parsec


type Env = IORef [(String, IORef LispVal)]

nullEnv :: IO Env
nullEnv = newIORef []

type IOThrowsError = ExceptT LispError IO


liftThrows :: ThrowsError a -> IOThrowsError a
liftThrows (Left err) = throwError err
liftThrows (Right val) = return val


runIOThrows :: IOThrowsError String -> IO String
runIOThrows action = runExceptT (trapError action) >>= return . extractValue


isBound :: Env -> String -> IO Bool
isBound envRef var = readIORef envRef >>= return . maybe False (const True) . lookup var


getVar :: Env -> String -> IOThrowsError LispVal
getVar envRef var  =  do env <- liftIO $ readIORef envRef
                         maybe (throwError $ UnboundVar "Getting an unbound variable" var)
                               (liftIO . readIORef)
                               (lookup var env)

setVar :: Env -> String -> LispVal -> IOThrowsError LispVal
setVar envRef var value = do env <- liftIO $ readIORef envRef
                             maybe (throwError $ UnboundVar "Setting an unbound variable" var)
                                   (liftIO . (flip writeIORef value))
                                   (lookup var env)
                             return value


defineVar :: Env -> String -> LispVal -> IOThrowsError LispVal
defineVar envRef var value = do
     alreadyDefined <- liftIO $ isBound envRef var
     if alreadyDefined
        then setVar envRef var value >> return value
        else liftIO $ do
             valueRef <- newIORef value
             env <- readIORef envRef
             writeIORef envRef ((var, valueRef) : env)
             return value


bindVars :: Env -> [(String, LispVal)] -> IO Env
bindVars envRef bindings = readIORef envRef >>= extendEnv bindings >>= newIORef
     where extendEnv bindings env = liftM (++ env) (mapM addBinding bindings)
           addBinding (var, value) = do ref <- newIORef value
                                        return (var, ref)


data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | String String
             | Bool Bool
             | PrimitiveFunc ([LispVal] -> ThrowsError LispVal)
             | Func { params :: [String], vararg :: (Maybe String),
                      body :: [LispVal],  closure :: Env }

instance Show LispVal where
    show (String contents) = "\"" ++ contents ++ "\""
    show (Atom name) = name
    show (Number contents) = show contents
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List contents) = "(" ++ unwordsList contents ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"
    show (PrimitiveFunc _) = "<primitive>"
    show (Func {params = args, vararg = varargs, body = body, closure = env}) =
        "(lambda (" ++ unwords (map show args) ++
            (case varargs of
                Nothing -> ""
                Just arg -> " . " ++ arg) ++ ") ...)"


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
