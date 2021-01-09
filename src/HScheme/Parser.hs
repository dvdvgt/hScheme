module HScheme.Parser where

import HScheme.Data

import Text.ParserCombinators.Parsec as P hiding ( spaces )
import Data.Char ( toUpper )
import Data.Ratio ( (%) )
import Data.Complex ( Complex((:+)) )
import Numeric ( readHex, readOct )
import Data.List ( foldl' )
import Control.Monad.Except

symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseBool :: Parser Value
parseBool = do
    try $ char '#'
    (char 't' >> return (Bool True)) <|> (char 'f' >> return (Bool False))

parseString :: Parser Value
parseString = do
    char '"'
    x <- many (noneOf "\"" <|> escapeChar)
    char '"'
    return $ String x
    where
        escapeChar :: Parser Char
        escapeChar = do
            oneOf "nrt\\" >>= \case
                'n' -> return '\n'
                'r' -> return '\r'
                't' -> return '\t'
                '\\'-> return '\\'

parseChar :: Parser Value
parseChar = do
    try $ string "#\\"
    chr <- (string "newline" <|> string "space") <|>
        do
            x <- anyChar 
            notFollowedBy alphaNum 
            return [x]
    return $ Character $ case chr of
        "\\" -> '\n'
        "newline" -> '\n'
        "space" -> ' '
        _ -> head chr

parseAtom :: Parser Value
parseAtom = do
    first <- letter <|> symbol
    rest <- many (letter <|> digit <|> symbol)
    let atom = first:rest
    return $ case atom of
        "#t" -> Bool True
        "#f" -> Bool False
        _    -> Atom atom

{-
    Parsing numbers
-}

parseDecimal :: Parser Value
parseDecimal = many1 digit >>= (return . Number . read)

parseDecimal' :: Parser Value
parseDecimal' = try $ string "#d" >> many1 digit >>= (return . Number . read)

-- | Parse hexadecimal number of form #x...
parseHex :: Parser Value
parseHex = try $ string "#x" >> many1 hexDigit >>= (return . Number . hexToDigit)
    where
        hexToDigit x = fst $ head $ readHex x

-- | Parse octadecimal number of form #o...
parseOct :: Parser Value
parseOct = try $ string "#o" >> many1 octDigit >>= (return . Number . octToDigit)
    where
        octToDigit x = fst $ head $ readOct x

-- | Parse binary of form #b...
parseBin :: Parser Value
parseBin = try $ string "#b" >> many1 (oneOf "10") >>= (return . Number . binToDigit)
    where
        binToDigit = foldl' binDigitToDigit 0
            where
                binDigitToDigit acc = \case
                    '1' -> 2 * acc + 1
                    '0' -> 2 * acc + 0

parseNumber :: Parser Value
parseNumber = do
    parseDecimal
    <|> parseDecimal'
    <|> parseHex
    <|> parseOct
    <|> parseBin

parseFloat :: Parser Value
parseFloat = try $ do
    x <- many1 digit
    y <- (:) <$> char '.' <*> many1 digit
    return $ Float $ read $ x <> y

parseRatio :: Parser Value
parseRatio = try $ do
    x <- many1 digit
    char '/'
    y <- many1 digit
    return $ Ratio (read x % read y)

parseComplex :: Parser Value
parseComplex = try $ do
    real <- parseFloat <|> parseNumber
    char '+'
    imiginary <- parseFloat <|> parseNumber
    char 'i'
    return $ Complex (toDouble real :+ toDouble imiginary)
    where
        toDouble (Float f) = realToFrac f
        toDouble (Number n) = realToFrac n

parseExpr :: Parser Value
parseExpr = parseAtom
    <|> parseString
    <|> parseChar
    <|> parseRatio
    <|> parseComplex
    <|> parseFloat
    <|> parseNumber
    <|> parseBool
    <|> parseQuoted
    <|> parseList

{-
    Recursive Parsers
-}

parseList :: Parser Value
parseList = do
    char '('
    x <- try parseSpacesList <|> parseDottedList
    char ')'
    return x

parseSpacesList :: Parser Value
parseSpacesList = List <$> sepBy parseExpr spaces

parseDottedList :: Parser Value
parseDottedList = do
    head <- endBy parseExpr spaces
    tail <- char '.' >> spaces >> parseExpr
    return $ DottedList head tail

parseQuoted :: Parser Value
parseQuoted = do
    char '\''
    x <- parseExpr
    return $ List [Atom "quote", x]

readExpr :: String -> ThrowsErr Value
readExpr input = case parse parseExpr "" input of
    Left err -> throwError $ Parser err
    Right val -> return val