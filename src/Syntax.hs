module Syntax
    ( Name
    , Expr (..)
    , Op (..)
    , parse
    ) where

import Data.ByteString.Short

type Name = ShortByteString

data Expr
    = Float Double
    | BinOp Op Expr Expr
    | Var Name
    | Call Name [Expr]
    | Function Name [Expr] Expr
    | Extern Name [Expr]
    deriving (Eq, Ord, Show)

data Op
    = Plus
    | Minus
    | Times
    | Divide
    deriving (Eq, Ord, Show)

parse :: String -> [Syntax.Expr]
parse input = undefined
