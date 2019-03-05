{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen (startCodegen, toBS) where

import           Control.Monad.State
import           Data.List
import qualified Data.ByteString.Char8           as C8
import qualified Data.ByteString.Short           as BS
import qualified Data.Map                        as Map

import           LLVM
import           LLVM.IRBuilder
import           LLVM.AST.Linkage
import           LLVM.AST.Global
import           LLVM.AST.ParameterAttribute
import           LLVM.AST.Type                   as Types
import qualified LLVM.AST                        as AST
import qualified LLVM.AST.Float                  as F
import qualified LLVM.AST.Constant               as C
import qualified LLVM.AST.FloatingPointPredicate as FPP
import qualified LLVM.AST.IntegerPredicate       as IP

-- import Data.ByteString.Char8 as B
-- import Data.ByteString.Short

import qualified Syntax
import qualified AstSelector

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
  , (Syntax.Eq, icmp IP.EQ)
  , (Syntax.NotEq, icmp IP.NE)
  ]

op = AST.ConstantOperand
ref t n = op $ C.GlobalReference t n
local t n = AST.LocalReference t (AST.Name n)

toBS :: AST.Name -> BS.ShortByteString
toBS (AST.Name n) = n
toBS (AST.UnName n) = BS.toShort $ C8.pack $ drop 1 $ show n

codegen :: Syntax.Expr -> IRBuilderT ModuleBuilder AST.Operand
codegen (Syntax.Decl (Syntax.Double v) _) = pure $ doublev v
codegen (Syntax.Decl (Syntax.Int v) _) = pure $ intv v
codegen (Syntax.Decl (Syntax.Str s) _) = globalStringPtr s =<< fresh
codegen (Syntax.Var v) = pure $ local int v
codegen (Syntax.If cond thenb elseb) = mdo
  tname <- fresh
  fname <- fresh
  ename <- fresh

  let condm = codegen cond
  condv <- condm
  condBr condv tname fname

  emitBlockStart tname
  tout <- foldl (\_ e -> codegen e) condm thenb
  br end

  emitBlockStart fname
  fout <- foldl (\_ e -> codegen e) condm elseb
  br end

  end <- block `named` (toBS ename)
  node <- phi [(tout, tname), (fout, fname)]
  return node
codegen (Syntax.While cond b) = mdo
  sname <- fresh
  bname <- fresh
  ename <- fresh
  
  br start
  start <- block `named` (toBS sname)
  let condm = codegen cond
  condv <- condm
  condBr condv bname ename

  emitBlockStart bname
  out <- foldl (\_ e -> codegen e) condm b
  br start

  emitBlockStart ename
  return out
codegen (Syntax.For name cond inc body) = codegen $ Syntax.While cond (body ++ [inc])
codegen (Syntax.Call fname fargs) = do
  -- operand <- gets (\x -> x)
  -- case operand of
    -- (AST.LocalReference opType opName) -> do
  args <- mapM (\a -> do
    arg <- codegen a
    return (arg, [])) fargs
  call (ref (funptr int [charptr]) (AST.Name fname)) args
    -- _ ->
codegen (Syntax.BinOp op lhs rhs) = case Map.lookup op binops of
  Just fn -> do
    lhs' <- codegen lhs
    rhs' <- codegen rhs
    fn lhs' rhs'
  Nothing -> error $ "no such operator: " ++ (show op)

buildFunction :: Syntax.Name -> [Syntax.Expr] -> AST.Type -> [Syntax.Expr] -> ModuleBuilder AST.Operand
buildFunction name args retType body = function' (AST.Name name) args' retType (bodyBuilder body)
  where
    args' = map arg args
    arg (Syntax.Arg n (PointerType (IntegerType 8) _)) = (charptr, LLVM.IRBuilder.ParameterName n, [ReadOnly, NonNull, NoAlias, NoCapture])
    -- arg (Syntax.Arg n t) = (t, LLVM.IRBuilder.ParameterName n, [])
    -- bodyBuilder b@(Syntax.While {}:es) args = bodyBuilderWithoutEntry args b
    bodyBuilder b args = mdo
      entry <- block `named` "entry"
    --   bodyBuilderWithoutEntry args b
    -- bodyBuilderWithoutEntry args b = mdo
      bodyOps <- sequence $ map (\e -> codegen e) b
      ret $ last bodyOps

startCodegen :: [Syntax.Expr] -> [Syntax.Expr] -> ModuleBuilder ()
startCodegen [] []     = return ()
startCodegen [] mainEs = do
  buildFunction "main" [] int $ reverse mainEs
  return ()
startCodegen (Syntax.Function name args retType body:es) mainEs = do
  buildFunction name args retType [body]
  startCodegen es mainEs
startCodegen (Syntax.Extern name argsType retType:es) mainEs = do
  extern' (AST.Name name) argsType retType
  startCodegen es mainEs
startCodegen (expr:es) mainEs = startCodegen es (expr:mainEs)
