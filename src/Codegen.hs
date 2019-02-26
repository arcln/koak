{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen (startCodegen) where

import           Control.Monad.State
import           Data.List
import qualified Data.Map          as Map

import           LLVM.IRBuilder
import           LLVM.AST.Type     as Types
import qualified LLVM.AST          as AST
import qualified LLVM.AST.Float    as F
import qualified LLVM.AST.Constant as C

import qualified Syntax

data CodegenState = CodegenState
  { symtab :: [(Syntax.Name, AST.Operand)]
  }

align = 8

char = i8
int = i32
long = i64
charptr = ptr i8
intptr = ptr i32
longptr = ptr i64

charv v = op $ C.Int 8 v
intv v = op $ C.Int 32 v
longv v = op $ C.Int 64 v
doublev v = op $ C.Float (F.Double v)

binops = Map.fromList
  [ (Syntax.Plus, fadd)
  , (Syntax.Minus, fsub)
  , (Syntax.Times, fmul)
  , (Syntax.Divide, fdiv)
  ]

op = AST.ConstantOperand
ref t n = op $ C.GlobalReference t n

codegen :: Syntax.Expr -> CodegenState -> IRBuilderT ModuleBuilder AST.Operand
codegen (Syntax.Float v) _ = pure $ doublev v
codegen (Syntax.Var v) s = case find (\sym -> fst sym == v) (symtab s) of
  Just x -> pure $ snd x
  Nothing -> error $ "no such symbol: " ++ (show v)
codegen (Syntax.Call fname fargs) s = do
  args <- mapM (\a -> do
    arg <- codegen a s
    return (arg, [])) fargs
  call (ref Types.double (AST.Name fname)) args
codegen (Syntax.BinOp op lhs rhs) s = case Map.lookup op binops of
  Just fn -> do
    lhs' <- codegen lhs s
    rhs' <- codegen rhs s
    fn lhs' rhs'
  Nothing -> error $ "no such operator: " ++ (show op)

buildArg :: Syntax.Expr -> IRBuilderT ModuleBuilder (Syntax.Name, AST.Operand)
buildArg (Syntax.Var name) = do
  var <- alloca Types.double Nothing align
  return (name, var)

buildFunction :: Syntax.Name -> [Syntax.Expr] -> Syntax.Expr -> ModuleBuilder ()
buildFunction name args body = do
  function (AST.Name name) args' Types.double $ \[] -> mdo
    stackArgs' <- sequence stackArgs
    entry <- block `named` "entry"
    ret =<< (codegen body $ CodegenState (stackArgs'))
  return ()
  where
    args' = map (\(Syntax.Var a) -> (Types.double, LLVM.IRBuilder.ParameterName a)) args
    stackArgs = map buildArg args

startCodegen :: [Syntax.Expr] -> ModuleBuilder ()
startCodegen [] = return ()
startCodegen (Syntax.Function name args body:es) = do
  buildFunction name args body
  startCodegen es
startCodegen (expr:es) = do
  buildFunction "main" [] expr
  startCodegen es
