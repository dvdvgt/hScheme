module HScheme.Data where

import Control.Monad.Except
import Data.Complex ( Complex )
import Text.ParserCombinators.Parsec ( ParseError )

data Value = Atom String
    | List [Value]
    | DottedList [Value] Value
    | Number Integer 
    | Float Double
    | Ratio Rational
    | Complex (Complex Double)
    | String String
    | Character Char
    | Bool Bool

instance Show Value where
    show (String s) = "\"" ++ s ++ "\""
    show (Character c) = "#\\" ++ [c]
    show (Atom name) = name
    show (Number n) = show n
    show (Float n) = show n
    show (Ratio n) = show n
    show (Complex n) = show n
    show (Bool True) = "#t"
    show (Bool False) = "#f"
    show (List xs) = "(" ++ unwordsList xs ++ ")"
    show (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ show tail ++ ")"

unwordsList :: [Value] -> String
unwordsList = unwords . map show

{-
    -- Error handling --
-}

data Error = NumArgs Integer [Value]
    | TypeMismatch String Value
    | Parser ParseError
    | BadSpecialForm String Value
    | NotFunction String String
    | UnboundVar String String
    | Default String

instance Show Error where
    show (NumArgs expected found)       = "Expected " ++ show expected 
                                        ++ " args; found values " ++ unwordsList found
    show (TypeMismatch expected found)  = "Invalid Type: expected " ++ expected
                                        ++ ", found " ++ show found
    show (Parser err)                    = "Parse error at " ++ show err
    show (BadSpecialForm message form)  = message ++ ": " ++ show form
    show (NotFunction message func)     = message ++ ": " ++ show func
    show (UnboundVar message varname)   = message ++ ": " ++ varname

type ThrowsErr = Either Error

trapErr :: (MonadError a m, Show a) => m String -> m String
trapErr action = catchError action (return . show)

extractValue :: ThrowsErr a -> a
extractValue (Right val) = val