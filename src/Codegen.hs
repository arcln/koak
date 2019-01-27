{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Codegen where

import           LLVM.IRBuilder
import           LLVM.AST.Type
import qualified LLVM.AST          as AST
import qualified LLVM.AST.Constant as C

import qualified Syntax

char = i8
int = i32
long = i64
charptr = ptr i8
intptr = ptr i32
longptr = ptr i64

charv v = op $ C.Int 8 v
intv v = op $ C.Int 32 v
longv v = op $ C.Int 64 v

op = AST.ConstantOperand
ref t n = op $ C.GlobalReference t n

genMain :: ModuleBuilder AST.Operand
genMain = do
  let atoiType = ptr $ FunctionType int [charptr] False
  extern "atoi" [charptr] int

  let addType = ptr $ FunctionType int [int, int] False
  function "add" [(i32, "a"), (i32, "b")] i32 $ \[a, b] -> mdo
    entry <- block `named` "entry"; do
      c <- add a b
      ret c

  function "main" [] int $ \[] -> mdo
    entry <- block `named` "entry"; do
      retval <- call (ref addType "add") [(intv 4, []), (intv 6, [])]
      ret retval

codegen :: Syntax.Expr -> ModuleBuilder AST.Operand
codegen _ = genMain