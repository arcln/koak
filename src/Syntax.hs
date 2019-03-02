module Syntax
    ( Name
    , Expr (..)
    , Op (..)
    , Syntax.parse
    ) where

import Control.Monad
import Control.Applicative
import Data.String.Encode
import Data.ByteString.Short

import Persa.Parser

type Name = ShortByteString

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | UnOp Op Expr
    | Var Name
    | Call Name [Expr]
    | Function Name [Arg] Expr
    | Extern Name [Expr]
    | Arg Name Type
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
--- unary          <- # unop unary | postfix
--- postfix        <- primary call_expr ?
--- dot            <- '.' !'.'

-- stmt <- kdefs * # eof
pStmt :: Parser [Expr]
pStmt = many pKdefs

-- kdefs <- 'def ' defs ';' | expressions ';'
pKdefs :: Parser Expr
pKdefs = do {
  spaces;
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
pDefs :: Parser Function
pDefs = do {
  (id, (args, _)) <- pPrototype;
  exprs <- pExpressions;
  return $ Function id args exprs
}

-- prototype <- ('unary ' . decimal_const ? | 'binary ' . decimal_const ? | identifier) prototype_args
pPrototype :: Parser (String, ([Arg], Type))
pPrototype = do {
  id <- pIdentifier;
  proto <- pPrototypeArgs;
  return (id, proto);
}

-- prototype_args <- '(' ( identifier ':' type ) * ')' ':' type
pPrototypeArgs :: Parser ([Arg], Type)
pPrototypeArgs = do {
  reserved "(";
  args <- many pArg
  reserved ")";
  reserved ":";
  ret <- pType;
  return (args, ret);
}
  where
    pArg :: Parser Var
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
    "int" = Int
    "double" = Double
    "void" = Void)
}

-- expressions <- for_expr | if_expr | while_expr | expression (':' expression ) *
pExpressions :: Parser [Expr]
pExpressions = pForExpr <|> pIfExpr <|> pWhileExpr <|> do {
  expr <- pExpressions;
  exprs <- many (do {
    reserved ":";
    return pExpression;
  });
  return expr:exprs;
}

--- expression <- unary (# binop ( unary | expression ) ) *
pExpression = undefined

pBinOp :: Parser BinOp
pBinOp = undefined

pUnOp :: Parser UnOp
pUnOp = undefined

-- call_expr <- '(' ( expression (',' expression ) *) ? ')'
pCallExpr :: Parser [Expr]
pCallExpr = parens $ optional (do {
  expr <- pExpression;
  exprs <- many (do {
    reserved ",";
    return pExpression;
  }) <|> return [];
  return expr:exprs;
})

-- primary <- identifier | literal | '(' expressions ')
pPrimary :: Parser Expr
pPrimary = pIdentifier <|> pLiteral <|> (do {

})

-- identifier <- [a - zA - Z ][ a - zA - Z0 -9]*
pIdentifier :: Parser Name
pIdentifier = do {
  f <- oneOf (['a'..'z'] ++ ['A'..'Z']);
  o <- many $ oneOf (['a'..'z'] ++ ['A'..'Z'] ++ ['0'..'9']);
  return $ convertString (f:o);
}

-- decimal_const <- [0 -9]+
pDecimalConst :: Parser Int
pDecimalConst = number

-- double_const <- ( decimal_const dot [0 -9]* | dot [0 -9]+ )
pDoubleConst :: Parser Double
pDoubleConst = fractional

-- literal <- decimal_const | double_const
pLiteral :: Parser Float
pLiteral = Float (pDecimalConst <|> pDoubleConst)

parse :: String -> [Syntax.Expr]
parse input = undefined

-- case runParser pXMLFile contents of
--     Left err -> putStrLn (show err) >>= (\_ -> exitWith (ExitFailure 84))
--     Right res -> putStrLn $ show $ toXSD res
