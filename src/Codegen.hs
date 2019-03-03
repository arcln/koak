{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen (startCodegen) where

import           Control.Monad.State
import           Data.List
import qualified Data.Map                        as Map

import           LLVM.IRBuilder
import           LLVM.AST.Type                   as Types
import qualified LLVM.AST                        as AST
import qualified LLVM.AST.Float                  as F
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.FloatingPointPredicate as FPP

-- import Data.ByteString.Char8 as B
-- import Data.ByteString.Short

import qualified Syntax

data CodegenState = CodegenState
  { symtab :: [AST.Operand]
  }

align = 8

bool = i1
char = i8
int = i32
long = i64
charptr = ptr i8
intptr = ptr i32
longptr = ptr i64
funptr ret args = ptr $ FunctionType ret args False

boolv v = op $ C.Int 1 v
charv v = op $ C.Int 8 v
intv v = op $ C.Int 32 v
longv v = op $ C.Int 64 v
doublev v = op $ C.Float (F.Double v)

binops = Map.fromList
  [ (Syntax.Plus, add)
  , (Syntax.Minus, fsub)
  , (Syntax.Times, mul)
  , (Syntax.Divide, fdiv)
  , (Syntax.Eq, fcmp FPP.OEQ)
  , (Syntax.NotEq, fcmp FPP.ONE)
  ]

op = AST.ConstantOperand
ref t n = op $ C.GlobalReference t n
local t n = AST.LocalReference t (AST.Name n)

codegen :: Syntax.Expr -> CodegenState -> IRBuilderT ModuleBuilder AST.Operand
codegen (Syntax.Decl (Syntax.Double v) _) _ = pure $ doublev v
codegen (Syntax.Decl (Syntax.Int v) _) _ = pure $ intv v
-- codegen (Syntax.If cond thenb elseb) s = mdo
--   condv <- codegen cond s
--   condBr condv "then" "else"
--   block `named` "then"; do
--     out <- codegen thenb s
--     br end
--   block `named` "else"; do
--     out <- codegen elseb s
--     br end
--   end <- block `named` "end"
--   return $ doublev 42
codegen (Syntax.Var v) _ = pure $ local int v
-- codegen (Syntax.Var v) s = case find (\sym -> name sym == v) (symtab s) of
--   Just x -> pure $ snd x
--   Nothing -> error $ "no such symbol: " ++ (show v)
codegen (Syntax.Call fname fargs) s = do
  args <- mapM (\a -> do
    arg <- codegen a s
    return (arg, [])) fargs
  call (ref (funptr int [int | _ <- [1..(length fargs)]]) (AST.Name fname)) args
codegen (Syntax.BinOp op lhs rhs) s = case Map.lookup op binops of
  Just fn -> do
    lhs' <- codegen lhs s
    rhs' <- codegen rhs s
    fn lhs' rhs'
  Nothing -> error $ "no such operator: " ++ (show op)

buildFunction :: Syntax.Name -> [Syntax.Expr] -> AST.Type -> [Syntax.Expr] -> ModuleBuilder AST.Operand
buildFunction name args retType body = function (AST.Name name) args' retType bodyBuilder
  where
    args' = map (\(Syntax.Arg n t) -> (t, LLVM.IRBuilder.ParameterName n)) args
    bodyBuilder args = mdo
      entry <- block `named` "entry"
      bodyOps <- sequence $ map (\e -> (codegen e $ CodegenState args)) body
      ret $ last bodyOps

startCodegen :: [Syntax.Expr] -> [Syntax.Expr] -> ModuleBuilder ()
startCodegen [] []      = return ()
startCodegen [] mainEs  = do
  buildFunction "main" [] int $ reverse mainEs
  return ()
startCodegen (Syntax.Function name args retType body:es) mainEs = do
  buildFunction name args retType [body]
  startCodegen es mainEs
startCodegen (expr:es) mainEs = startCodegen es (expr:mainEs)
