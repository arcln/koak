module Persa.Parser
  where

import Control.Monad
import Control.Monad.State
import Control.Applicative
import Debug.Trace

import Data.Char
import Data.Typeable

import Persa.Reader

newtype Parser a = Parser {
  parse :: Reader -> State [((Int, Int), String)] (Either String a, Reader)
}

item :: Parser Char
item = Parser $ \r ->
  case r of
    (Reader [] rc) -> return (Left "no more items to parse", (Reader [] rc));
    r -> return (Right (charReader r), tailReader r)

unit :: a -> Parser a
unit a = Parser $ \r -> return (Right a, r)

bind :: Parser a -> (a -> Parser b) -> Parser b
bind pa fapb = Parser $ \r -> do
  res <- parse pa r
  case res of
    (Left e, rs) -> do
      return (Left e, rs)
    (Right res, rs) -> parse (fapb res) rs

_map :: (a -> b) -> Parser a -> Parser b
_map f pa = Parser $ \r -> do
  res <- parse pa r
  case res of
    (Left e, rs) -> return (Left e, rs)
    (Right res, rs) -> return (Right $ f res, rs)

_mapButDiff :: Parser (a -> b) -> Parser a -> Parser b
_mapButDiff pfab pa = Parser $ \r -> do
  res <- parse pfab r
  case res of
    (Left e, rs) -> return (Left e, rs)
    (Right f, rs) -> do
      rr <- parse pa rs
      case rr of
        (Left e, rs) -> return (Left e, rs)
        (Right a, rs2) -> return (Right $ f a, rs2)

failure :: Parser a
failure = Parser $ \r -> return (Left "MonadPlus.mzero", (Reader [] []))

combine :: Parser a -> Parser a -> Parser a
combine p1 p2 = Parser $ \s -> return (Left "idk", s) -- I don't know

_or :: Parser a -> Parser a -> Parser a
_or p1 p2 = Parser $ \s -> do
  r <- parse p1 s
  case r of
    (Left _, _') -> do
      parse p2 s
    res -> return res

instance Functor Parser where
  fmap = _map

instance Applicative Parser where
  pure = return
  (<*>) = _mapButDiff

instance Monad Parser where
  (>>=) = bind
  return = unit

instance MonadPlus Parser where
  mzero = failure
  mplus = combine

instance Alternative Parser where
  empty = mzero
  (<|>) = _or

satisfy :: (Char -> Bool) -> Parser Char
satisfy f = Parser $ \r ->
  case r of
    (Reader [] rc) -> return (Left "empty string", (Reader [] rc))
    r -> case f (charReader r) of
        True -> return (Right (charReader r), tailReader r)
        False -> do {
          errs <- get;
          put $ errs ++ [((snd $ headReader r), take 10 $ stringReader r)];
          return (Left "syntax error", r);
        }

oneOf :: [Char] -> Parser Char
oneOf s = satisfy (flip elem s)

char :: Char -> Parser Char
char c = satisfy (c ==)

notChar :: Char -> Parser Char
notChar c = satisfy (c /=)

natural :: Parser Integer
natural = read <$> some digit

string :: String -> Parser String
string [] = return []
string (c:cs) = do {char c; string cs; return(c:cs)}

token :: Parser a -> Parser a
token pa = do {
  spaces;
  a <- pa;
  spaces;
  return a;
}

reserved :: String -> Parser String
reserved str = token $ string str

spaces :: Parser String
spaces = many $ satisfy (isSpace)

digit :: Parser Char
digit = satisfy isDigit

number :: Parser Int
number = do {
  s <- (string "+" <|> string "-") <|> return [];
  n <- some digit;
  return $ read ((if s == "+" then [] else s) ++ n);
}

fractional :: Parser Double
fractional = do {
  s <- (string "+" <|> string "-") <|> return [];
  n <- some digit;
  r <- (do{
    d <- char '.';
    d' <- some digit;
    return (d:d')}) <|> return "";
  return $ read ((if s == "+" then [] else s) ++ n ++ r);
}

parens :: Parser a -> Parser a
parens pa = do {
  reserved "(";
  r <- pa;
  reserved ")";
  return r;
}

getDeepestErr :: [((Int, Int), String)] -> ((Int, Int), String)
getDeepestErr errs = foldr1 (\a b -> if a `deeperThan` b then a else b) errs
  where
    deeperThan ((x, y), _) ((x2, y2), _') | x == x2 = y > y2
                                          | otherwise = x > x2

runParser :: Parser a -> String -> Either ((Int, Int), String) a
runParser p s = evalState ss []
  where
    ss = do
      r <- parse p (makeReader s)
      errs <- get
      case r of
        (Right res, (Reader [] _)) -> return $ Right res
        (Right _, rs) -> return $ Left ((-1, -1), "string not empty")
        (Left err, str) -> return $ Left $ getDeepestErr errs
