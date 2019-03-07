module Testcases where

import Data.String
import Data.String.Encode
import LLVM.AST.Type         as LLVM

import Syntax

decl :: LLVM.Type -> String -> Value -> Expr
decl t name x = Block [Decl t (convertString name) (Data x)]

strDecl x     = decl (LLVM.ptr LLVM.i8) "str" (Str x)
doubleDecl x  = decl LLVM.double "double" (Double x)
intDecl x     = decl LLVM.i32 "int" (Int x)

parseError (pos, err) = decl (LLVM.ptr LLVM.i8) (fromString ("Parse Error at " ++ show pos)) (Str err)

int42'              = intDecl 42
int42               = [int42']
double42'           = doubleDecl 42
double42            = [double42']
floatsEndingWith42  = [doubleDecl 0, double42']

fnNoArgs name = Function name' []
  where name' = fromString name :: Name

fnNoArgs42_0' name = fnNoArgs name LLVM.double double42'
fnNoArgs42_0 = [fnNoArgs42_0' "noArguments"]

fnNoArgs42' name = fnNoArgs name LLVM.i32 int42'
fnNoArgs42 = [fnNoArgs42' "noArguments"]

fnCallNoArgs fn name =
  [ fn name
  , Call (fromString name :: Name) []
  ]

fnCallNoArgs42 = fnCallNoArgs fnNoArgs42' "fnNoArgs42"
fnCallNoArgs42_0 = fnCallNoArgs fnNoArgs42_0' "fnNoArgs42_0"

fnCallDoubleArg =
  [ Function calleeName [arg] LLVM.double arg
  , Call calleeName double42
  ]
  where
    arg         = Var (fromString "arg" :: Name)
    calleeName  = (fromString "returnFirstArg" :: Name)

printHelloWorld = undefined -- TODO implement with an AST a function printing hello world
