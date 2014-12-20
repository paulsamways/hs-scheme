module Scheme where

import Control.Monad
import Control.Monad.Except
import Text.Parsec hiding (spaces)
import Text.Parsec.String

data LispValue = LispAtom String
               | LispList [LispValue]
               | LispDottedList [LispValue] LispValue
               | LispNumber Integer
               | LispString String
               | LispBool Bool
                 deriving (Eq, Ord)

instance Show LispValue where
  show (LispAtom x) = x
  show (LispList xs) = "(" ++ unwords (map show xs) ++ ")"
  show (LispDottedList xs y) = "(" ++ unwords (map show xs) ++ " . " ++ show y ++ ")"
  show (LispNumber x) = show x
  show (LispString x) = "\"" ++ x ++ "\""
  show (LispBool True) = "#t"
  show (LispBool False) = "#f"

data LispError = ErrorArity Integer [LispValue]
               | ErrorTypeMismatch String LispValue
               | ErrorParse ParseError
               | ErrorBadSpecialForm String LispValue
               | ErrorNotFunction String String
               | ErrorUnboundVar String String
               | ErrorDefault String

instance Show LispError where
  show (ErrorArity expects given) = "Arity error: Expected "
                                       ++ show expects
                                       ++ " arguments but was given "
                                       ++ show (length given)
                                       ++ " ("
                                       ++ unwords (map show given)
                                       ++ ")."
  show (ErrorTypeMismatch expected given) = "Type mismatch error: Expected "
                                            ++ expected
                                            ++ " but was given "
                                            ++ show given
                                            ++ "."
  show (ErrorParse e) = "Parse error: " ++ show e
  show (ErrorBadSpecialForm s v) = "Bad special form error: " ++ s ++ ": " ++ show v
  show (ErrorNotFunction s f) = "Not function error: " ++ s ++ ": " ++ show f
  show (ErrorUnboundVar s name) = "Unbound var error: " ++ s ++ ": " ++ name
  show (ErrorDefault s) = s

--instance Except LispError where
--  noMsg = ErrorDefault "An error has occured"
--  strMsg = ErrorDefault

type ThrowsError = Either LispError

trapError action = catchError action (return . show)

extractValue :: ThrowsError a -> a
extractValue (Right v) = v


symbol :: Parser Char
symbol = oneOf "!#$%&|*+-/:<=>?@^_~"

spaces :: Parser ()
spaces = skipMany1 space

parseAtom :: Parser LispValue
parseAtom = do
  first <- letter <|> symbol
  rest <- many $ letter <|> digit <|> symbol
  let atom = first:rest
  return $ case atom of
    "#t" -> LispBool True
    "#f" -> LispBool False
    _    -> LispAtom atom

parseString :: Parser LispValue
parseString = do
  _ <- char '"'
  x <- many $ noneOf "\""
  _ <- char '"'
  return $ LispString x

parseNumber :: Parser LispValue
parseNumber = liftM (LispNumber . read) $ many1 digit

parseList :: Parser LispValue
parseList = liftM LispList $ sepBy parseExpr spaces

parseDottedList :: Parser LispValue
parseDottedList = do
  x <- endBy parseExpr spaces
  y <- char '.' >> spaces >> parseExpr
  return $ LispDottedList x y

parseQuoted :: Parser LispValue
parseQuoted = do
  _ <- char '\''
  x <- parseExpr
  return $ LispList [LispAtom "quote", x]

parseExpr :: Parser LispValue
parseExpr = parseAtom <|> parseString <|> parseNumber <|> parseQuoted <|> do
  _ <- char '('
  x <- try parseList <|> parseDottedList
  _ <- char ')'
  return x
            
readExpr :: String -> ThrowsError LispValue
readExpr input = case parse parseExpr "lisp" input of
  Left err -> throwError $ ErrorParse err
  Right v -> return v

car :: [LispValue] -> ThrowsError LispValue
car [LispList (x:_)] = return x
car [LispDottedList (x:_) _] = return x
car [badArg] = throwError $ ErrorTypeMismatch "pair" badArg
car badArgList = throwError $ ErrorArity 1 badArgList

cdr :: [LispValue] -> ThrowsError LispValue
cdr [LispList (_:xs)] = return $ LispList xs
cdr [LispDottedList [_] x] = return x
cdr [LispDottedList (_:xs) x] = return $ LispDottedList xs x
cdr [x] = throwError $ ErrorTypeMismatch "pair" x
cdr x = throwError $ ErrorArity 1 x

unpackNum :: LispValue -> ThrowsError Integer
unpackNum (LispNumber x) = return x
unpackNum v@(LispString x) = case reads x :: [(Integer, String)] of
                            []         -> throwError $ ErrorTypeMismatch "number" v
                            ((x', _):_) -> return x'
unpackNum (LispList [x]) = unpackNum x
unpackNum v = throwError $ ErrorTypeMismatch "number" v

unpackStr :: LispValue -> ThrowsError String
unpackStr (LispString s) = return s
unpackStr (LispNumber s) = return $ show s
unpackStr (LispBool s)   = return $ show s
unpackStr notString  = throwError $ ErrorTypeMismatch "string" notString

unpackBool :: LispValue -> ThrowsError Bool
unpackBool (LispBool b) = return b
unpackBool notBool  = throwError $ ErrorTypeMismatch "boolean" notBool

numericBinOp :: (Integer -> Integer -> Integer) -> [LispValue] -> ThrowsError LispValue
numericBinOp _ [] = throwError $ ErrorArity  2 []
numericBinOp _ v@[_] = throwError $ ErrorArity 2 v
numericBinOp op args = mapM unpackNum args >>= return . LispNumber . foldl1 op

boolBinOp :: (LispValue -> ThrowsError a) -> (a -> a -> Bool) -> [LispValue] -> ThrowsError LispValue
boolBinOp unpacker op args = if length args /= 2
                             then throwError $ ErrorArity 2 args
                             else do
                               left <- unpacker $ args !! 0
                               right <- unpacker $ args !! 1
                               return $ LispBool $ left `op` right

numBoolBinop  = boolBinOp unpackNum
strBoolBinop  = boolBinOp unpackStr
boolBoolBinop = boolBinOp unpackBool

arity2Func :: (LispValue -> LispValue -> a) -> (a -> LispValue) -> [LispValue] -> ThrowsError LispValue
arity2Func f g [a,b] = return $ g $ f a b
arity2Func _ _ args = throwError $ ErrorArity 2 args

primitives :: [(String, [LispValue] -> ThrowsError LispValue)]
primitives = [("+", numericBinOp (+))
             ,("-", numericBinOp (-))
             ,("*", numericBinOp (*))
             ,("/", numericBinOp div)
             ,("mod", numericBinOp mod)
             ,("quotient", numericBinOp quot)
             ,("remainder", numericBinOp rem)
             ,("=", arity2Func (==) LispBool)
             ,("<", numBoolBinop (<))
             ,(">", numBoolBinop (>))
             ,("/=", numBoolBinop (/=))
             ,(">=", numBoolBinop (>=))
             ,("<=", numBoolBinop (<=))
             ,("&&", boolBoolBinop (&&))
             ,("||", boolBoolBinop (||))
             ,("string=?", strBoolBinop (==))
             ,("string<?", strBoolBinop (<))
             ,("string>?", strBoolBinop (>))
             ,("string<=?", strBoolBinop (<=))
             ,("string>=?", strBoolBinop (>=))
             ,("car", car)
             ,("cdr", cdr)]

apply :: String -> [LispValue] -> ThrowsError LispValue
apply f args = maybe (throwError $ ErrorNotFunction "Unrecognized primitive function args" f)
               ($ args) 
               (lookup f primitives)

eval :: LispValue -> ThrowsError LispValue
eval (LispAtom x) = undefined
eval (LispList [LispAtom "quote", v]) = return v
eval (LispList [LispAtom "if", p, conseq, alt]) = do
  result <- eval p
  case result of
   LispBool False -> eval alt
   otherwise -> eval conseq
eval (LispList (LispAtom f : args)) = mapM eval args >>= apply f
eval (LispList xs) = undefined
eval (LispDottedList xs y) = undefined
eval v@(LispNumber _) = return v
eval v@(LispString _) = return v
eval v@(LispBool _) = return v
eval v = throwError $ ErrorBadSpecialForm "Unrecognized special form" v
