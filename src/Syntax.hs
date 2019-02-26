module Syntax
    ( Name
    , Expr (..)
    , Op (..)
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
