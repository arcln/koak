module Testcases where

import Data.String
import LLVM.AST.Type         as LLVM

import Syntax

doubleDecl x = Decl (Double x) Nothing

double42'           = doubleDecl 42
double42            = [double42']
floatsEndingWith42  = [doubleDecl 0, double42']

functionWithNoArgument' name = Function name' [] LLVM.double double42'
  where name' = fromString name :: Name

functionWithNoArgument = [functionWithNoArgument' "noArguments"]

functionCallWithNoArgument =
  [ functionWithNoArgument' calleeName
  , Call (fromString calleeName :: Name) []
  ]
  where calleeName = "test"

functionCallWithDoubleArgument =
  [ Function calleeName [arg] LLVM.double arg
  , Call calleeName double42
  ]
  where
    arg         = Var (fromString "arg" :: Name)
    calleeName  = (fromString "returnFirstArg" :: Name)

printHelloWorld = undefined -- TODO implement with an AST a function printing hello world
