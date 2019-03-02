module Syntax
    ( Name
    , Expr (..)
    , Op (..)
    , Syntax.parse
    ) where

import Prelude
import Control.Monad
import Control.Applicative
import Data.String.Encode
import Data.ByteString.Short
import Debug.Trace

import Persa.Parser

type Name = ShortByteString

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | UnOp Op Expr
    | Var Name
    | Call Name [Expr]
    | Function Name [Expr] Expr
    | Extern Name [Expr]
    | Arg Name Type
    | Block [Expr]
    deriving (Eq, Ord, Show)

data Op
    = Plus
    | Substract
    | Times
    | Divide
    | Not
    | Minus
    deriving (Eq, Ord, Show)

data Type
    = Int
    | Double
    | Void
    deriving (Eq, Ord, Show)

--- for_expr       <- 'for ' identifier '=' expression ',' identifier '<' expression ',' expression 'in ' expressions
--- if_expr        <- 'if ' expression 'then ' expressions ('else ' expressions ) ?
--- while_expr     <- 'while ' expression 'do ' expressions

-- stmt <- kdefs * # eof
pStmt :: Parser [Expr]
pStmt = do {
  k <- many pKdefs;
  return k;
}

-- kdefs <- 'def ' defs ';' | expressions ';'
pKdefs :: Parser Expr
pKdefs = do {
  reserved "def";
  def <- pDefs;
  reserved ";";
  return def;
} <|> do {
  spaces;
  expr <- pExpressions;
  reserved ";";
  return expr;
}

-- defs <- prototype expressions
pDefs :: Parser Expr
pDefs = do {
  (id, (args, _)) <- pPrototype;
  exprs <- pExpressions;
  return $ Function id args exprs
}

-- prototype <- ('unary ' . decimal_const ? | 'binary ' . decimal_const ? | identifier) prototype_args
pPrototype :: Parser (Name, ([Expr], Type))
pPrototype = do {
  id <- pIdentifier;
  proto <- pPrototypeArgs;
  return (id, proto);
}

-- prototype_args <- '(' ( identifier ':' type ) * ')' ':' type
pPrototypeArgs :: Parser ([Expr], Type)
pPrototypeArgs = do {
  reserved "(";
  args <- many pArg;
  reserved ")";
  reserved ":";
  ret <- pType;
  return (args, ret);
}
  where
    pArg :: Parser Expr
    pArg = do {
      id <- pIdentifier;
      reserved ":";
      t <- pType;
      return $ Arg id t;
    }

-- type <- 'int ' | 'double ' | 'void '
pType :: Parser Type
pType = do {
  t <- reserved "int" <|> reserved "double" <|> reserved "void";
  return (case t of
    "int" -> Int
    "double" -> Double
    "void" -> Void)
}

-- expressions <- for_expr | if_expr | while_expr | expression (':' expression ) *
pExpressions :: Parser Expr
pExpressions = do { --pForExpr <|> pIfExpr <|> pWhileExpr <|> do {
  expr <- pExpression;
  exprs <- many (do {
    reserved ":";
    pExpression;
  });
  return $ case exprs of
    [] -> expr
    otherwise -> Block (expr:exprs)
}

--- expression <- unary (# binop ( unary | expression ) ) *
pExpression :: Parser Expr
pExpression = pChain pTerm pBinOpLow pTerm

pTerm :: Parser Expr
pTerm = pChain pUnary pBinOpHigh (pUnary <|> pExpression)


-- call_expr <- '(' ( expression (',' expression ) *) ? ')'
pCallExpr :: Parser [Expr]
pCallExpr = parens (do {
  ma <- optional pCallArgs;
  return (case ma of
    Just args -> args
    Nothing -> []);
})
  where
    pCallArgs :: Parser [Expr]
    pCallArgs = do {
      expr <- pExpression;
      exprs <- many (do {
        reserved ",";
        pExpression
      }) <|> return [];
      return (expr:exprs);
    }

-- unary <- # unop unary | postfix
pUnary :: Parser Expr
pUnary = (do {
  op <- pUnOp;
  un <- pUnary;
  return $ op un;
}) <|> pPostfix

-- postfix <- primary call_expr ?
pPostfix :: Parser Expr
pPostfix = do {
  p <- pPrimary;
  c <- optional pCallExpr;
  return (case c of
    Just ce -> case p of
      Var name -> Call name ce
      otherwise -> p
    Nothing -> p)
}

-- primary <- identifier | literal | '(' expressions ')
pPrimary :: Parser Expr
pPrimary = (fmap (\n -> Var n) pIdentifier) <|> pLiteral <|> do {
  reserved "(";
  exprs <- pExpressions;
  reserved ")";
  return exprs;
}

-- identifier <- [a - zA - Z ][ a - zA - Z0 -9]*
pIdentifier :: Parser Name
pIdentifier = do {
  f <- oneOf (['a'..'z'] ++ ['A'..'Z']);
  o <- many $ oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']);
  return $ convertString (f:o);
}

-- decimal_const <- [0 -9]+
pDecimalConst :: Parser Double
pDecimalConst = do {
  n <- number;
  return $ fromIntegral n;
}

-- double_const <- ( decimal_const dot [0 -9]* | dot [0 -9]+ )
pDoubleConst :: Parser Double
pDoubleConst = fractional

-- literal <- decimal_const | double_const
pLiteral :: Parser Expr
pLiteral = do {
  f <- pDecimalConst <|> pDoubleConst;
  return $ Float f;
}

pChain :: Parser Expr -> Parser (Expr -> Expr -> Expr) -> Parser Expr -> Parser Expr
pChain expr op expr2 = do {e <- expr; chain e}
  where
    chain e = (do {
      o <- op;
      e2 <- expr2;
      chain (o e e2);
    }) <|> return e

pOperator :: String -> a -> Parser a
pOperator c op = reserved c >> return op

pBinOpLow :: Parser (Expr -> Expr -> Expr)
pBinOpLow = pOperator "+" (BinOp Plus) <|> pOperator "-" (BinOp Substract)

pBinOpHigh :: Parser (Expr -> Expr -> Expr)
pBinOpHigh = pOperator "*" (BinOp Times) <|> pOperator "/" (BinOp Divide)

pUnOp :: Parser (Expr -> Expr)
pUnOp = pOperator "!" (UnOp Not) <|> pOperator "-" (UnOp Minus)

parse :: String -> Either ParseError [Syntax.Expr]
parse input = runParser pStmt input
