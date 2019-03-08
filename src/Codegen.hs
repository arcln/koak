{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen (startCodegen, inferType, toBS, ) where

import Debug.Trace

import           Control.Monad.State
import           Data.Word
import           Data.List
import           Data.Word
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
import qualified LLVM.AST.AddrSpace              as A
import           LLVM.IRBuilder.Internal.SnocList

import qualified Syntax

align = 8

bool = i1
char = i8
int = i32
long = i64
charptr = ptr i8
intptr = ptr i32
longptr = ptr i64
funptr ret args = ptr $ FunctionType ret args True

boolv v = op $ C.Int 1 v
charv v = op $ C.Int 8 v
intv v = op $ C.Int 32 v
longv v = op $ C.Int 64 v
doublev v = op $ C.Float (F.Double v)

binops :: MonadIRBuilder m => Map.Map Syntax.Op (AST.Operand -> AST.Operand -> m AST.Operand)
binops = Map.fromList
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

op = AST.ConstantOperand
ref t n = op $ C.GlobalReference t n
local t n = AST.LocalReference t (AST.Name n)

getIR :: IRBuilderT ModuleBuilder ModuleBuilderState
getIR = liftModuleState $ get

getMB :: ModuleBuilder ModuleBuilderState
getMB = liftModuleState $ get

toBS :: AST.Name -> BS.ShortByteString
toBS (AST.Name n) = n
toBS (AST.UnName n) = BS.toShort $ C8.pack $ drop 1 $ show n

getFuncDefByName :: ModuleBuilderState -> AST.Name -> Maybe AST.Definition
getFuncDefByName (ModuleBuilderState builderDefs _) name = find byName (getSnocList builderDefs)
  where
    byName (AST.GlobalDefinition (AST.Function _ _ _ _ _ _ typeName _ _ _ _ _ _ _ _ _ _)) = typeName == name
    byName _ = False

getFnRetType :: AST.Definition -> AST.Type
getFnRetType (AST.GlobalDefinition (AST.Function _ _ _ _ _ funcType _ _ _ _ _ _ _ _ _ _ _)) = funcType

getFuncType :: AST.Definition -> AST.Type
getFuncType (AST.GlobalDefinition (AST.Function _ _ _ _ _ funcType _ (params, isVaArgs) _ _ _ _ _ _ _ _ _)) = asPointer $ AST.FunctionType funcType paramsTypes isVaArgs
  where
    paramsTypes = map (\(AST.Parameter t _ _) -> t) params
    asPointer fn = PointerType (fn) (A.AddrSpace 0)

getVarTypeByName :: ModuleBuilderState -> AST.Name -> AST.Type
getVarTypeByName (ModuleBuilderState _ builderTypeDefs) name = case Map.lookup name builderTypeDefs of
  Just t -> t
  Nothing -> error $ "could not find type of variable " ++ show name

getFnTypeByName :: ModuleBuilderState -> AST.Name -> AST.Type
getFnTypeByName mbs name = case getFuncDefByName mbs name of
  Just func -> getFuncType func
  Nothing   -> error $ "could not find type of function " ++ show name

getFnRetTypeByName :: ModuleBuilderState -> AST.Name -> AST.Type
getFnRetTypeByName mbs name = case getFuncDefByName mbs name of
  Just func -> getFnRetType func
  Nothing   -> error $ "could not find return type of function " ++ show name

inferTypes :: ModuleBuilderState -> String -> Syntax.Expr -> Syntax.Expr -> Type
inferTypes s name lhs rhs
  | lhsType == rhsType  = lhsType
  | otherwise           = error $ name ++ " types mismatch " ++ (show lhsType) ++ " " ++ (show rhsType)
  where
    lhsType = inferType s lhs
    rhsType = inferType s rhs

inferType :: ModuleBuilderState -> Syntax.Expr -> Type
inferType s (Syntax.Block []) = int
inferType s (Syntax.Block b) = last $ map (inferType s) b
inferType s (Syntax.Data (Syntax.Double _)) = Types.double
inferType s (Syntax.Data (Syntax.Int _)) = int
inferType s (Syntax.Data (Syntax.Str _)) = charptr
inferType s (Syntax.Decl t _ _) = t
inferType s (Syntax.Assign _ expr) = inferType s expr
inferType s (Syntax.Var name) = getVarTypeByName s (AST.Name name)
inferType s (Syntax.If _ thenb elseb) = inferTypes s "if-else" thenb elseb
inferType s (Syntax.While _ b) = inferType s b
inferType s (Syntax.For _ _ _ b) = inferType s b
inferType s (Syntax.Call fname _) = getFnRetTypeByName s (AST.Name fname)
inferType s (Syntax.BinOp op lhs rhs) = inferTypes s (show op) lhs rhs

codegen :: Syntax.Expr -> IRBuilderT ModuleBuilder AST.Operand
codegen (Syntax.Block []) = do
  unreachable
  return $ intv 0
codegen (Syntax.Block b) = do
  ops <- sequence $ map codegen b
  return $ seq ops (last ops)
codegen (Syntax.Data (Syntax.Double v)) = pure $ doublev v
codegen (Syntax.Data (Syntax.Int v)) = pure $ intv v
codegen (Syntax.Data (Syntax.Str s)) = do
  name <- fresh
  globalStringPtr s name
  let str = ref (ptr (ArrayType (fromIntegral $ length s + 1) char)) name
  ptr <- bitcast str charptr
  return ptr
codegen (Syntax.Decl t n e) = do
  var <- (alloca t (Just $ intv 1) align) `named` n
  init <- codegen e
  store var align init
  val <- load var align
  return val
codegen (Syntax.Assign n e) = do
  var <- codegen $ Syntax.Var n
  init <- codegen e
  store var align init
  val <- load var align
  return $ intv 0
codegen v'@(Syntax.Var v) = do
  state <- getIR
  let varType = inferType state v'
  let var = local varType v
  case varType of
    (PointerType _ _) -> load var align
    _                 -> return var
codegen (Syntax.If cond thenb elseb) = mdo
  tname <- fresh
  fname <- fresh
  ename <- fresh

  condv <- codegen cond
  condBr condv tname fname

  emitBlockStart tname
  tout <- codegen thenb -- foldl (\_ e -> codegen e) condm thenb
  br end

  emitBlockStart fname
  fout <- codegen elseb
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
  condv <- codegen cond
  condBr condv bname ename

  emitBlockStart bname
  out <- codegen b -- foldl (\_ e -> codegen e) condm b
  br start

  emitBlockStart ename
  return out
codegen (Syntax.For init cond inc body) = do
  sname <- fresh
  bname <- fresh
  ename <- fresh

  _ <- codegen init
  start <- block `named` (toBS sname)
  condv <- codegen cond
  condBr condv bname ename

  emitBlockStart bname
  out <- codegen body
  _ <- codegen inc
  br start

  emitBlockStart ename
  return out
codegen (Syntax.Call fname fargs) = do
  args <- mapM (\a -> do
    arg <- codegen a
    return (arg, [])) fargs
  state <- getIR
  let funcType = getFnTypeByName state (AST.Name fname)
  call (ref (funcType) (AST.Name fname)) args
codegen (Syntax.BinOp op lhs rhs) = case Map.lookup op binops of
  Just fn -> do
    lhs' <- codegen lhs
    rhs' <- codegen rhs
    fn lhs' rhs'
  Nothing -> error $ "no such operator: " ++ (show op)
codegen expr = error $ "uncomputable node found in AST: " ++ (show expr)

buildFunction :: Syntax.Name -> [Syntax.Expr] -> AST.Type -> Syntax.Expr -> ModuleBuilder AST.Operand
buildFunction name args retType body = function' (AST.Name name) args' retType bodyBuilder
  where
    args' = map arg args
    -- arg (Syntax.Arg n (PointerType (IntegerType 8) _)) = (charptr, LLVM.IRBuilder.ParameterName n, [ReadOnly, NonNull, NoAlias, NoCapture])
    arg (Syntax.Arg n t) = (t, LLVM.IRBuilder.ParameterName n, [])
    -- bodyBuilder b@(Syntax.While {}:es) args = bodyBuilderWithoutEntry args b
    bodyBuilder args = mdo
      entry <- block `named` "entry"
    --   bodyBuilderWithoutEntry args b
    -- bodyBuilderWithoutEntry args b = mdo
      ret =<< codegen body

startCodegen :: [Syntax.Expr] -> [Syntax.Expr] -> ModuleBuilder ()
startCodegen [] []     = return ()
startCodegen [] mainEs = do
  state <- getMB
  buildFunction "main" [] (inferType state insts) insts
  return ()
    where insts = Syntax.Block $ reverse mainEs
startCodegen (Syntax.Function name args retType body:es) mainEs = do
  buildFunction name args retType body
  startCodegen es mainEs
startCodegen (Syntax.Extern name argsType retType True:es) mainEs = do
  externVarArgs (AST.Name name) argsType retType
  startCodegen es mainEs
startCodegen (Syntax.Extern name argsType retType False:es) mainEs = do
  extern' (AST.Name name) argsType retType
  startCodegen es mainEs
startCodegen (expr:es) mainEs = startCodegen es (expr:mainEs)

-- jit $ [ (Extern ( BS.toShort $ C8.pack "puts") [Types.ptr Types.i8]) Types.i32 False, (Call (BS.toShort $ C8.pack "puts") [Var (BS.toShort $ C8.pack "str")]), (Decl (Str "aze") (Just $ BS.toShort $ C8.pack "str")) ]
-- jit $ [ (Decl (Str "aze") (Just $ BS.toShort $ C8.pack "str")), (Extern ( BS.toShort $ C8.pack "puts") [Types.ptr Types.i8]) Types.i32 False, (Call (BS.toShort $ C8.pack "puts") [Var (BS.toShort $ C8.pack "str")]) ]
-- jit $ [ (Extern ( BS.toShort $ C8.pack "puts") [Types.ptr Types.i8]) Types.i32 False, (Call (BS.toShort $ C8.pack "puts") [Var (BS.toShort $ C8.pack "str")]) ]
