module Syntax
    ( Name
    , Expr (..)
    , Op (..)
    , Type (..)
    , Value (..)
    , Syntax.parse
    ) where

import Prelude
import Control.Monad
import Control.Applicative
import Data.String.Encode
import Data.ByteString.Short
import Debug.Trace
import Persa.Parser

import LLVM.AST.Type         as LLVM

type Name = ShortByteString

data Expr
    = Decl Value (Maybe Name)
    | BinOp Op Expr Expr
    | UnOp Op Expr
    | Var Name
    | Call Name [Expr]
    | Function Name [Expr] Type Expr
    | Extern Name [Type] Type
    | Arg Name Type
    | Block [Expr]
    | If Expr [Expr] [Expr]
    | While Expr [Expr]
    | For Name Expr Expr [Expr]
    deriving (Eq, Ord, Show)

data Op
    = Plus
    | Minus
    | Times
    | Divide
    | Not
    | Eq
    | NotEq
    deriving (Eq, Ord, Show)

data Value
    = Void
    | Int Integer
    | Double Double
    | Str String
    | Array Value
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
pKdefs = pExtern <|> do {
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
  (id, (args, ret)) <- pPrototype;
  exprs <- pExpressions;
  return $ Function id args ret exprs
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
  t <- reserved "int" <|> reserved "double" <|> reserved "void" <|> reserved "string";
  return (case t of
    "int" -> LLVM.i32
    "double" -> LLVM.double
    "string" -> LLVM.ptr LLVM.i8
    "void" -> LLVM.void)
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
pExpression = pConstStr <|> pChain pTerm pBinOpLow pTerm

pConstStr :: Parser Expr
pConstStr = do {
  reserved "\"";
  str <- many $ notChar '\"';
  reserved "\"";
  return $ Decl (Str str) Nothing;
}

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
pDecimalConst :: Parser (String, Double)
pDecimalConst = do {
  n <- number;
  return ("int", fromIntegral n);
}

-- double_const <- ( decimal_const dot [0 -9]* | dot [0 -9]+ )
pDoubleConst :: Parser (String, Double)
pDoubleConst = do {
  d <- Persa.Parser.double;
  return ("double", d);
}

-- literal <- decimal_const | double_const
pLiteral :: Parser Expr
pLiteral = do {
  (t, n) <- pDoubleConst <|> pDecimalConst;
  return $ case t of
    "int" -> Decl (Int $ round n) Nothing
    "double" -> Decl (Double n) Nothing
}

-- BONUS: extern C calls
pExtern :: Parser Expr
pExtern = do {
  reserved "using";
  name <- pIdentifier;
  reserved "(";
  argsType <- many pType;
  reserved ")";
  reserved ":";
  ret <- pType;
  reserved ";";
  return $ Extern name argsType ret
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
pBinOpLow = pOperator "+" (BinOp Plus) <|> pOperator "-" (BinOp Minus)

pBinOpHigh :: Parser (Expr -> Expr -> Expr)
pBinOpHigh = pOperator "*" (BinOp Times) <|> pOperator "/" (BinOp Divide)

pUnOp :: Parser (Expr -> Expr)
pUnOp = pOperator "!" (UnOp Not) <|> pOperator "-" (UnOp Minus)

parse :: String -> Either ParseError [Syntax.Expr]
parse input = runParser pStmt input
