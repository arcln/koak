{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen (startCodegen, inferType, toBS) where

import Debug.Trace

import           Control.Monad.State
import           Data.Word
import           Data.List
import           Data.Word
import           Data.Maybe
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
import qualified LLVM.AST.FloatingPointPredicate as FP
import qualified LLVM.AST.IntegerPredicate       as IP
import qualified LLVM.AST.AddrSpace              as A
import           LLVM.IRBuilder.Internal.SnocList

import           Helpers
import           Inferrer
import qualified Syntax

align = 8

iunops :: MonadIRBuilder m => Map.Map Syntax.Op (AST.Operand -> m AST.Operand)
iunops = Map.fromList
  [ (Syntax.Minus, sub $ intv 0)
  , (Syntax.Not, icmp IP.EQ $ intv 0)
  ]

funops :: MonadIRBuilder m => Map.Map Syntax.Op (AST.Operand -> m AST.Operand)
funops = Map.fromList
  [ (Syntax.Minus, fsub $ doublev 0)
  , (Syntax.Not, fcmp FP.OEQ $ doublev 0)
  ]

ibinops :: MonadIRBuilder m => Map.Map Syntax.Op (AST.Operand -> AST.Operand -> m AST.Operand)
ibinops = Map.fromList
  [ (Syntax.Plus, add)
  , (Syntax.Minus, sub)
  , (Syntax.Times, mul)
  , (Syntax.Divide, sdiv)
  , (Syntax.Eq, icmp IP.EQ)
  , (Syntax.NotEq, icmp IP.NE)
  , (Syntax.Lt, icmp IP.SLT)
  , (Syntax.Gt, icmp IP.SGT)
  , (Syntax.Gte, icmp IP.SGE)
  , (Syntax.Lte, icmp IP.SLE)
  ]

fbinops :: MonadIRBuilder m => Map.Map Syntax.Op (AST.Operand -> AST.Operand -> m AST.Operand)
fbinops = Map.fromList
  [ (Syntax.Plus, fadd)
  , (Syntax.Minus, fsub)
  , (Syntax.Times, fmul)
  , (Syntax.Divide, fdiv)
  , (Syntax.Eq, fcmp FP.OEQ)
  , (Syntax.NotEq, fcmp FP.ONE)
  , (Syntax.Lt, fcmp FP.OLT)
  , (Syntax.Gt, fcmp FP.OGT)
  , (Syntax.Gte, fcmp FP.OGE)
  , (Syntax.Lte, fcmp FP.OLE)
  ]

getIR :: IRBuilderT ModuleBuilder ModuleBuilderState
getIR = liftModuleState $ get

toBS :: AST.Name -> BS.ShortByteString
toBS (AST.UnName n) = BS.toShort $ C8.pack $ drop 1 $ show n

zipVaArgs :: [a] -> [b] -> [(a, Maybe b)]
zipVaArgs as bs = zipVaArgs' as bs []
  where
    zipVaArgs' [] [] out = out
    zipVaArgs' (a:as) [] out = zipVaArgs' as [] $ out ++ [(a, Nothing)]
    zipVaArgs' (a:as) (b:bs) out = zipVaArgs' as bs $ out ++ [(a, Just b)]

buildFunction :: Syntax.Expr-> ModuleBuilder AST.Operand
buildFunction f@(Syntax.Function name args retType body) = function' (AST.Name name) args' retType bodyBuilder
  where
    args' = map arg args
    arg (Syntax.Arg n t) = (t, LLVM.IRBuilder.ParameterName n, [])
    bodyBuilder args = mdo
      entry <- block `named` "entry"
      ret =<< codegen f body

codegen :: Syntax.Expr -> Syntax.Expr -> IRBuilderT ModuleBuilder AST.Operand
codegen ast (Syntax.Block []) = do
  unreachable
  return $ intv 0
codegen ast (Syntax.Block b) = do
  ops <- sequence $ map (codegen ast) b
  return $ seq ops (last ops)
codegen ast (Syntax.Data (Syntax.Double v)) = pure $ doublev v
codegen ast (Syntax.Data (Syntax.Int v)) = pure $ intv v
codegen ast (Syntax.Data (Syntax.Str s)) = do
  name <- fresh
  globalStringPtr s name
  let str = ref (ptr (ArrayType (fromIntegral $ length s + 1) char)) name
  ptr <- bitcast str charptr
  return ptr
codegen ast (Syntax.Decl t n e) = do
  var <- (alloca t (Just $ intv 1) (align :: Word32)) `named` n
  init <- codegen ast e
  store var align init
  val <- load var align
  return val
codegen ast (Syntax.Assign n e) = do
  let varType = inferTypeFromAst [ast] (Syntax.Var n)
  let var = local varType n
  case varType of
    (PointerType _ _) -> do
      init <- codegen ast e
      store var align init
      val <- load var align
      return val
    _                 -> error $ "cannot assign constant " ++ (show n)
codegen ast v'@(Syntax.Var v) = do
  let varType = inferTypeFromAst [ast] v'
  let var = local varType v
  case varType of
    (PointerType _ _) -> load var align
    _                 -> return var
codegen ast (Syntax.If cond thenb elseb) = mdo
  tname <- fresh
  fname <- fresh
  ename <- fresh

  condv <- codegen ast cond >>= (`as` bool)
  condBr condv tname fname

  emitBlockStart tname
  tout <- codegen ast thenb
  br end

  emitBlockStart fname
  fout <- codegen ast elseb
  br end

  end <- block `named` (toBS ename)
  node <- phi [(tout, tname), (fout, fname)]
  return node
codegen ast (Syntax.While cond b) = mdo
  sname <- fresh
  bname <- fresh
  ename <- fresh

  br start
  start <- block `named` (toBS sname)
  condv <- codegen ast cond >>= (`as` bool)
  condBr condv bname ename

  emitBlockStart bname
  _ <- codegen ast b -- foldl (\_ e -> codegen ast e) condm b
  br start

  emitBlockStart ename
  return $ intv 0
codegen ast (Syntax.For init cond inc body) = mdo
  sname <- fresh
  bname <- fresh
  ename <- fresh

  _ <- codegen ast init
  br start
  start <- block `named` (toBS sname)
  condv <- codegen ast cond >>= (`as` bool)
  condBr condv bname ename

  emitBlockStart bname
  _ <- codegen ast body
  _ <- codegen ast inc
  br start

  emitBlockStart ename
  return $ intv 0
codegen ast (Syntax.Call fname fargs) = do
  state <- getIR
  let funcArgs = getFnArgsTypeByName state (AST.Name fname)
  let funcRet = getFnRetTypeByName state (AST.Name fname)
  let funcType = getFnTypeByName state (AST.Name fname)
  args <- mapM genArg $ zipVaArgs fargs funcArgs
  call (ref (funcType) (AST.Name fname)) args
    where
      genArg (a, t) = do
        arg <- case t of
          Just t' -> codegen ast a >>= (`as` t')
          _ -> codegen ast a
        return (arg, [])
codegen ast e@(Syntax.BinOp op lhs rhs) = do
  state <- getIR
  let opType = inferTypeInternal (Just [ast]) (Just state) e
  let retType = getStrongType (Just state) [ast] e
  let binops = if opType == int then ibinops else fbinops
  case Map.lookup op binops of
    Just fn -> do
      lhs' <- codegen ast lhs >>= (`as` opType)
      rhs' <- codegen ast rhs >>= (`as` opType)
      res  <- fn lhs' rhs'
      case retType of
        IntegerType 1 -> res `as` int
        _ -> res `as` retType
    Nothing -> error $ "no such operator: " ++ (show op)
codegen ast e@(Syntax.UnOp op rhs) = do
  state <- getIR
  let opType = inferTypeInternal (Just [ast]) (Just state) e
  let retType = getStrongType (Just state) [ast] e
  let unops = if opType == int then iunops else funops
  case Map.lookup op unops of
    Just fn -> do
      rhs'  <- codegen ast rhs >>= (`as` opType)
      res   <- fn rhs'
      case retType of
        IntegerType 1 -> res `as` int
        _ -> res `as` retType
    Nothing -> error $ "no such operator: " ++ (show op)
codegen ast expr = error $ "uncomputable node found in AST: " ++ (show expr)

startCodegen :: [Syntax.Expr] -> ([Syntax.Expr], [Syntax.Expr]) -> ModuleBuilder (Maybe Type)
startCodegen [] ([], _)       = return Nothing
startCodegen [] (mainEs, ast) = do
  let retType = inferTypeFromAst ast insts
  buildFunction $ Syntax.Function "main" [] (getRetType retType) insts
  return $ Just (getRetType retType)
    where
      insts = Syntax.Block $ reverse mainEs
      getRetType (PointerType (IntegerType 32) _) = int
      getRetType t = t
startCodegen (f@(Syntax.Function {}):es) state = do
  buildFunction f
  startCodegen es state
startCodegen (Syntax.Extern name argsType retType True:es) state = do
  externVarArgs (AST.Name name) argsType retType
  startCodegen es state
startCodegen (Syntax.Extern name argsType retType False:es) state = do
  extern' (AST.Name name) argsType retType
  startCodegen es state
startCodegen (expr:es) (mainEs, ast) = startCodegen es (expr:mainEs, ast)
