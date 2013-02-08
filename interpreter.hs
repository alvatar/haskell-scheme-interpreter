module Main where
import System.Environment
import Text.ParserCombinators.Parsec hiding (spaces)
import Monad
import Numeric
import Ratio
import Complex
import Data.Array
import Control.Monad.Error

-- Parsing
symbol :: Parser Char
symbol = oneOf "!$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

data LispVal = Atom String
             | List [LispVal]
             | DottedList [LispVal] LispVal
             | Number Integer
             | Float Double
             | Ratio Rational
             | Complex (Complex Double)
             | Character Char
             | String String
             | Bool Bool
             | Vector (Array Int LispVal)
             | Nil ()

-- Booleans
parseBool :: Parser LispVal
parseBool = do string "#"
               x <- oneOf "tf"
               return $ case x of
                             't' -> Bool True
                             'f' -> Bool False

-- Characters
parseCharacter :: Parser LispVal
parseCharacter = do try $ string "#\\"
                    char <- try (string "newline" <|> string "space")
                            <|> do x <- anyChar
                                   notFollowedBy alphaNum
                                   return [x]
                    return $ Character $ case char of
                      "space" -> ' '
                      "newline" -> '\n'
                      otherwise -> char !! 0

-- Strings
parseEscapedString :: Parser Char
parseEscapedString = do char '\\'
                        x <- oneOf "\\\"nrt"
                        return $ case x of
                          '\\' -> x
                          '"' -> x
                          'n' -> '\n'
                          'r' -> '\r'
                          't' -> '\t'

parseString :: Parser LispVal
parseString = do char '"'
                 x <- many $ parseEscapedString <|> noneOf "\\\""
                 char '"'
                 return $ String x

-- Atoms
parseAtom :: Parser LispVal
parseAtom = do first <- letter <|> symbol
               rest <- many (letter <|> digit <|> symbol)
               return $ Atom ([first] ++ rest)

-- Numbers

parsePlainDecimal :: Parser LispVal
parsePlainDecimal = many1 digit >>= return . Number . read

parseDecimal :: Parser LispVal
parseDecimal = do try $ string "#d"
                  x <- many1 digit
                  (return . Number . read) x

bin2dig  = bin2dig' 0
bin2dig' digint "" = digint
bin2dig' digint (x:xs) = let old = 2 * digint + (if x == '0' then 0 else 1) in
                         bin2dig' old xs
parseBinary :: Parser LispVal
parseBinary = do try $ string "#b"
                 x <- many1 (oneOf "01")
                 return (Number (bin2dig x))

parseOctal :: Parser LispVal
parseOctal = do try $ string "#o"
                x <- many1 octDigit
                (return . Number . fst) $ readOct x !! 0

parseHexadecimal :: Parser LispVal
parseHexadecimal = do try $ string "#h"
                      x <- many1 hexDigit
                      (return . Number . fst) $ readHex x !! 0

parseNumber :: Parser LispVal
parseNumber = do number <- parsePlainDecimal <|> parseDecimal <|> parseBinary <|> parseOctal <|> parseHexadecimal
                 return $ number

parseFloat :: Parser LispVal
parseFloat = do a <- many1 digit
                char '.'
                b <- many1 digit
                return $ Float (fst . head $ readFloat (a ++ "." ++ "b"))

parseRatio :: Parser LispVal
parseRatio = do a <- many1 digit
                char '/'
                b <- many1 digit
                return $ Ratio ((read a) % (read b))

toDouble :: LispVal -> Double
toDouble(Float f) = f
toDouble(Number n) = fromIntegral n

parseComplex :: Parser LispVal
parseComplex = do a <- (try parseFloat <|> parseDecimal)
                  char '+'
                  b <- (try parseFloat <|> parseDecimal)
                  char 'i'
                  return $ Complex (toDouble a :+ toDouble b)

-- Lists
--  parseList :: Parser LispVal
--  parseList = liftM List $ sepBy parseExpr spaces

--  parseDottedList :: Parser LispVal
--  parseDottedList = do head <- endBy parseExpr spaces
                     --  tail <- char '.' >> spaces >> parseExpr
                     --  return $ DottedList head tail

parseAnyList :: Parser LispVal
parseAnyList = do char '('
                  optional spaces
                  head <- sepEndBy parseExpr spaces
                  tail <- (char '.' >> spaces >> parseExpr) <|> return (Nil ())
                  optional spaces
                  char ')'
                  return $ case tail of
                    (Nil ()) -> List head
                    otherwise -> DottedList head tail

parseQuoted :: Parser LispVal
parseQuoted = do char '\''
                 x <- parseExpr
                 return $ List [Atom "quote", x]

parseQuasiQuoted :: Parser LispVal
parseQuasiQuoted = do char '`'
                      x <- parseExpr
                      return $ List [Atom "quasiquote", x]

parseUnquote :: Parser LispVal
parseUnquote = do char ','
                  x <- parseExpr
                  return $ List [Atom "unquote", x]

-- Vectors
parseVector :: Parser LispVal
parseVector = do arrayValues <- sepBy parseExpr spaces
                 return $ Vector (listArray (0, (length arrayValues - 1)) arrayValues)

-- Expressions
parseExpr :: Parser LispVal
parseExpr = parseAtom
          <|> parseString
          <|> try parseRatio
          <|> try parseFloat
          <|> try parseComplex
          <|> try parseNumber
          <|> try parseBool
          <|> try parseCharacter
          <|> parseQuoted
          <|> parseQuasiQuoted
          <|> parseUnquote
          <|> try (do string "#("
                      x <- parseVector
                      char ')'
                      return x)
          <|> parseAnyList

-- Print LispVal
showVal :: LispVal -> String
showVal (String contents) = "\"" ++ contents ++ "\""
showVal (Atom name) = name
showVal (Number contents) = show contents
showVal (Bool True) = "#t"
showVal (Bool False) = "#f"
showVal (List contents) = "(" ++ unwordsList contents ++ ")"
showVal (DottedList head tail) = "(" ++ unwordsList head ++ " . " ++ showVal tail ++ ")"

unwordsList :: [LispVal] -> String
unwordsList = unwords . map showVal

instance Show LispVal where show = showVal

-- Evaluation
eval :: LispVal -> ThrowsError LispVal
eval val@(String _) = return val
eval val@(Number _) = return val
eval val@(Bool _) = return val
eval (List [Atom "quote", val]) = return val
eval (List (Atom func : args)) = mapM eval args >>= apply func
eval badForm = throwError $ BadSpecialForm "Unrecognized special form" badForm

apply :: String -> [LispVal] -> ThrowsError LispVal
apply func args = maybe (throwError $ NotFunction "Unrecognized primitive function args" func)
                        ($ args)
                        (lookup func primitives)

primitives :: [(String, [LispVal] -> ThrowsError LispVal)]
primitives = [("+", numericBinop (+)),
              ("-", numericBinop (-)),
              ("*", numericBinop(*)),
              ("/", numericBinop div),
              ("mod", numericBinop mod),
              ("quotient", numericBinop quot),
              ("remainder", numericBinop rem)]
              --  ("symbol?", unaryOp symbolp),
              --  ("string?", unaryOp stringp),
              --  ("number?", unaryOp numberp),
              --  ("bool?", unaryOp boolp),
              --  ("list?", unaryOp listp),
              --  ("vector?", unaryOp vectorp)]

--  unaryOp :: (LispVal -> LispVal) -> [LispVal] -> ThrowsError LispVal
--  unaryOp f [v] = f v

--  symbolp, numberp, stringp, boolp, listp, vectorp :: LispVal -> ThrowsError LispVal
--  symbolp (Atom _) = return Bool True
--  symbolp _ = return Bool False
--  numberp (Number _) = Bool True
--  numberp _ = Bool False
--  stringp (String _) = Bool True
--  stringp _ = Bool False
--  boolp (Bool _) = Bool True
--  boolp _ = Bool False
--  listp (List _) = Bool True
--  listp (DottedList _ _) = Bool True
--  listp _ = Bool False
--  vectorp (Vector _) = Bool True
--  vectorp _ = Bool False

numericBinop :: (Integer -> Integer -> Integer) -> [LispVal] -> ThrowsError LispVal
numericBinop op singleVal@[_] = throwError $ NumArgs 2 singleVal
numericBinop op params = mapM unpackNum params >>= return . Number . foldl1 op

unpackNum :: LispVal -> ThrowsError Integer
unpackNum (Number n) = return n
unpackNum (String n) = let parsed = reads n in
                           if null parsed
                              then throwError $ TypeMismatch "number" $ String n
                              else return $ fst $ parsed !! 0
unpackNum (List [n]) = unpackNum n
npackNum notNum = throwError $ TypeMismatch "number" notNum

-- Error handling
data LispError = NumArgs Integer [LispVal]
               | TypeMismatch String LispVal
               | Parser ParseError
               | BadSpecialForm String LispVal
               | NotFunction String String
               | UnboundVar String String
               | Default String

showError :: LispError -> String
showError (UnboundVar message varname) = message ++ ": " ++ varname
showError (BadSpecialForm message form) = message ++ ": " ++ show form
showError (NotFunction message func) = message ++ ": " ++ show func
showError (NumArgs expected found) = "Expected " ++ show expected ++ " args; found values " ++ unwordsList found
showError (TypeMismatch expected found) = "Invalid type: expected " ++ expected ++ ", found " ++ show found
showError (Parser parseErr) = "Parse error at " ++ show parseErr

instance Show LispError where show = showError

instance Error LispError where
  noMsg = Default "An error occurred"
  strMsg = Default

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right val) = val

readExpr :: String -> ThrowsError LispVal
readExpr input = case parse parseExpr "lisp" input of
    Left err -> throwError $ Parser err
    Right val -> return val

main :: IO ()
main = do
    args <- getArgs
    evaled <- return $ liftM show $ readExpr (args !! 0) >>= eval
    putStrLn $ extractValue $ trapError evaled

