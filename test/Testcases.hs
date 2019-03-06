module Testcases where

import Data.String
import LLVM.AST.Type         as LLVM

import Syntax

strDecl x = Decl (Str x) Nothing
doubleDecl x = Decl (Double x) Nothing
intDecl x = Decl (Int x) Nothing

parseError (pos, err) = Decl (Str err) (Just (fromString ("Parse Error at " ++ show pos) :: Name))

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
