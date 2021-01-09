module HScheme.Eval where

import HScheme.Data
import Control.Monad.Except
import Data.Maybe

eval :: Value -> ThrowsErr Value
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func

apply :: String -> [Value] -> ThrowsErr Value 
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [Value] -> ThrowsErr Value)]
primitives = [
    -- Basic operations
      ("+", numericBinaryOp (+))
      , ("-", numericBinaryOp (-))
      , ("*", numericBinaryOp (*))
      , ("/", numericBinaryOp div)
      , ("mod", numericBinaryOp mod)
      , ("quotient", numericBinaryOp quot)
      , ("remainder", numericBinaryOp rem)
    -- Type testing
      , ("symbol?", unaryOp isSymbol)
      , ("string?", unaryOp isString)
      , ("number?", unaryOp isNumber)
      , ("bool?", unaryOp isBool)
      , ("list?", unaryOp isList)
    -- Symbol handling procedure
      , ("string->symbol", unaryOp (\case (String s) -> Atom s; _ -> Bool False))
      , ("symbol->string", unaryOp (\case (Atom s) -> String s; _ -> Bool False))
    ]

numericBinaryOp :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsErr Value
numericBinaryOp op [] = throwError $ NumArgs 2 []
numericBinaryOp op val@[_] = throwError $ NumArgs 2 val
numericBinaryOp op args = mapM unpackNum args >>= return . Number . foldl1  op

unpackNum :: Value -> ThrowsErr Integer
unpackNum (Number n) = return n

unaryOp :: (Value -> Value) -> [Value] -> ThrowsErr Value
unaryOp f [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v

{-
    Type testing procedures
-}

isSymbol :: Value -> Value
isSymbol (Atom _) = Bool True
isSymbol _ = Bool False

isString :: Value -> Value 
isString (String _) = Bool True
isString _ = Bool False

isNumber :: Value -> Value
isNumber (Number _) = Bool True
isNumber _ = Bool False

isBool :: Value -> Value
isBool (Bool _) = Bool True
isBool _ = Bool False

isList :: Value -> Value
isList (List _) = Bool True
isList (DottedList _ _) = Bool True
isList _ = Bool False

{-
    Symbol handling procedures
-}