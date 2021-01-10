module HScheme.Eval where

import HScheme.Data
import Control.Monad.Except
import Data.Maybe

{-
    TODO:
        - Add support for different numeric types like Ratio or Complex
-}

eval :: Value -> ThrowsErr Value
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Float _) = return val
eval val@(Bool _) = return val
eval val@(Atom _) = return val
eval (List [Atom "quote", val]) = return val
eval (List [Atom "if", pred, conseq, alt]) = eval pred >>= \case
    Bool False -> eval alt
    _ -> eval conseq
eval form@(List (Atom "cond" : clauses))
    | null clauses = throwError $ BadSpecialForm "No true clause in cond expression" form
    | otherwise = case head clauses of
        List [Atom "else", expr] -> eval expr
        List [pred, expr] -> eval $ List [Atom "if", pred, expr, List (Atom "cond" : tail clauses)]
eval form@(List (Atom "case" : key : clauses))
    | null clauses = throwError $ BadSpecialForm "No true clause in cond expression" form
    | otherwise = case head clauses of
        List (Atom "else" : exprs) -> mapM eval exprs >>= return . last
        List ((List datums) : exprs) -> do
            result <- eval key
            equality <- mapM (\x -> eqv [result, x]) datums
            if Bool True `elem` equality
                then mapM eval exprs >>= return . last
                else eval $ List (Atom "case" : key : tail clauses)
        _ -> throwError $ BadSpecialForm "Invalid case expression" form
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [Value] -> ThrowsErr Value 
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [Value] -> ThrowsErr Value)]
primitives = 
    [
    -- Basic operations
    ("+", numBinaryOp (+))
    , ("-", numBinaryOp (-))
    , ("*", numBinaryOp (*))
    , ("/", numBinaryOp div)
    , ("mod", numBinaryOp mod)
    , ("quotient", numBinaryOp quot)
    , ("remainder", numBinaryOp rem)
    -- Bool operations
    , ("=", numBoolBinaryOp (==))
    , ("<", numBoolBinaryOp (<))
    , (">", numBoolBinaryOp (>))
    , ("/=", numBoolBinaryOp (/=))
    , (">=", numBoolBinaryOp (>=))
    , ("<=", numBoolBinaryOp (<=))
    , ("&&", boolBinaryOp (&&))
    , ("||", boolBinaryOp (||))
    , ("string=?", strBoolBinaryOp (==))
    , ("string<?", strBoolBinaryOp (<))
    , ("string>?", strBoolBinaryOp (>))
    , ("string<=?", strBoolBinaryOp (<=))
    , ("string>=?", strBoolBinaryOp (>=))
    -- Type testing
    , ("symbol?", unaryOp isSymbol)
    , ("string?", unaryOp isString)
    , ("number?", unaryOp isNumber)
    , ("bool?", unaryOp isBool)
    , ("list?", unaryOp isList)
    -- Symbol handling procedure
    , ("string->symbol", unaryOp stringToSymbol)
    , ("symbol->string", unaryOp symbolToString)
    -- List primitives
    , ("cons", cons)
    , ("eqv?", eqv)
    ]

numBinaryOp :: (Integer -> Integer -> Integer) -> [Value] -> ThrowsErr Value
numBinaryOp op [] = throwError $ NumArgs 2 []
numBinaryOp op val@[_] = throwError $ NumArgs 2 val
numBinaryOp op args = mapM unpackNum args >>= return . Number . foldl1  op

unpackNum :: Value -> ThrowsErr Integer
unpackNum = \case
    Number n -> return n
    wrongType -> throwError $ TypeMismatch "number" wrongType

unpackStr :: Value -> ThrowsErr String
unpackStr = \case
    String s -> return s
    Number n -> return $ show n
    Bool b -> return $ show b
    wrongType -> throwError $ TypeMismatch "string" wrongType

unpackBool :: Value -> ThrowsErr Bool
unpackBool = \case
    Bool b -> return b
    wrongType -> throwError $ TypeMismatch "boolean" wrongType

unaryOp :: (Value -> Value) -> [Value] -> ThrowsErr Value
unaryOp f [] = throwError $ NumArgs 1 []
unaryOp f [v] = return $ f v

{-
    Boolean operations
-}

binaryOp :: (Value -> ThrowsErr a) -> (b -> Value) -> (a -> a -> b) -> [Value] -> ThrowsErr Value
binaryOp unpacker resultType op args
    | length args /= 2 = throwError $ NumArgs 2 args
    | otherwise = do
        lhs <- unpacker $ head args
        rhs <- unpacker $ args !! 1
        return $ resultType $ lhs `op` rhs

numBoolBinaryOp :: (Integer -> Integer -> Bool) -> [Value] -> ThrowsErr Value
numBoolBinaryOp = binaryOp unpackNum Bool

strBoolBinaryOp :: (String -> String -> Bool) -> [Value] -> ThrowsErr Value
strBoolBinaryOp = binaryOp unpackStr Bool

boolBinaryOp :: (Bool -> Bool -> Bool) -> [Value] -> ThrowsErr Value
boolBinaryOp = binaryOp unpackBool Bool

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
stringToSymbol :: Value -> Value 
stringToSymbol = \case
    String s -> Atom s
    _ -> Bool False

symbolToString :: Value -> Value
symbolToString = \case
    Atom s -> String s
    _ -> Bool False

{-
    List primitives
-}

-- | Extracts first element.
car :: [Value] -> ThrowsErr Value
car [List (x:xs)] = return x
car [DottedList (x:xs) _] = return x
car [badArg] = throwError $ TypeMismatch "pair" badArg
car badArgList = throwError $ NumArgs 1 badArgList

-- | Extracts second element.
cdr :: [Value] -> ThrowsErr Value
cdr [List (x:xs)] = return $ List xs
cdr [DottedList [_] x] = return x
cdr [DottedList (_:xs) x] = return $ DottedList xs x
cdr [badArg] = throwError $ TypeMismatch "pair" badArg
cdr badArgList = throwError $ NumArgs 1 badArgList

cons :: [Value] -> ThrowsErr Value
cons [x, List []] = return $ List [x]
cons [x, List xs] = return $ List $ x : xs
cons [x, DottedList xs last] = return $ DottedList (x : xs) last
cons [x, y] = return $ DottedList [x] y
cons badArgs = throwError $ NumArgs 2 badArgs

eqv :: [Value] -> ThrowsErr Value
eqv [Bool arg1, Bool arg2] = return $ Bool $ arg1 == arg2
eqv [Number arg1, Number arg2] = return $ Bool $ arg1 == arg2
eqv [String arg1, String arg2] = return $ Bool $ arg1 == arg2
eqv [Atom arg1, Atom arg2] = return $ Bool $ arg1 == arg2
eqv [DottedList xs x, DottedList ys y] = eqv [List (x:xs), List(y:ys)]
eqv [List xs, List ys] = return $ Bool $ all (== True) $ zipWith (==) xs ys
eqv [_, _] = throwError $ Default "eqv?: unspecified"
eqv args@(_:_:_) = throwError $ NumArgs 2 args
eqv _ = throwError $ Default "eqv?: Unsupported types"