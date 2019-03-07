{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen (startCodegen, inferType, toBS) where

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
  , (Syntax.Minus, fsub)
  , (Syntax.Times, mul)
  , (Syntax.Divide, fdiv)
  , (Syntax.Eq, icmp IP.EQ)
  , (Syntax.NotEq, icmp IP.NE)
  ]

op = AST.ConstantOperand
ref t n = op $ C.GlobalReference t n
local t n = AST.LocalReference t (AST.Name n)
get' = liftModuleState $ get

toBS :: AST.Name -> BS.ShortByteString
toBS (AST.Name n) = n
toBS (AST.UnName n) = BS.toShort $ C8.pack $ drop 1 $ show n

getFuncDefByName :: ModuleBuilderState -> AST.Name -> Maybe AST.Definition
getFuncDefByName (ModuleBuilderState builderDefs _) name = find byName (getSnocList builderDefs)
  where
    byName (AST.GlobalDefinition (AST.Function _ _ _ _ _ _ typeName _ _ _ _ _ _ _ _ _ _)) = typeName == name
    byName _ = False

getFuncType :: AST.Definition -> AST.Type
getFuncType (AST.GlobalDefinition (AST.Function _ _ _ _ _ funcType _ (params, isVaArgs) _ _ _ _ _ _ _ _ _)) = getFuncPtr $ AST.FunctionType funcType paramsTypes isVaArgs
  where
    paramsTypes = map (\(AST.Parameter t _ _) -> t) params
    getFuncPtr fn = PointerType (fn) (A.AddrSpace 0)

getTypeByName :: ModuleBuilderState -> AST.Name -> Maybe AST.Type
getTypeByName mbs name = case getFuncDefByName mbs name of
  Just func -> Just $ getFuncType func
  Nothing   -> Nothing

inferTypes :: String -> Syntax.Expr -> Syntax.Expr -> Type
inferTypes name lhs rhs
  | lhsType == rhsType  = lhsType
  | otherwise             = error $ name ++ " types mismatch " ++ (show lhsType) ++ " " ++ (show rhsType)
  where
    lhsType = inferType lhs
    rhsType = inferType rhs

inferType :: Syntax.Expr -> Type
inferType (Syntax.Block b) = last $ map inferType b
inferType (Syntax.Data (Syntax.Double _)) = Types.double
inferType (Syntax.Data (Syntax.Int _)) = int
inferType (Syntax.Data (Syntax.Str _)) = charptr
inferType (Syntax.Decl t _ _) = t
inferType (Syntax.Assign _ expr) = inferType expr
inferType (Syntax.Var name) = int -- FIXME
inferType (Syntax.If _ thenb elseb) = inferTypes "if-else" thenb elseb
inferType (Syntax.While _ b) = inferType b
inferType (Syntax.For _ _ _ b) = inferType b
inferType (Syntax.Call fname _) = int -- FIXME
inferType (Syntax.BinOp op lhs rhs) = inferTypes (show op) lhs rhs

codegen :: Syntax.Expr -> IRBuilderT ModuleBuilder AST.Operand
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
codegen (Syntax.Var v) = do
  let var = local charptr v   -- FIXME: dehardcode type
  val <- load var align     -- FIXME: compute me only if var type is pointer
  return val
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
  fout <- codegen elseb -- foldl (\_ e -> codegen e) condm elseb
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
codegen (Syntax.For name cond (Syntax.Block inc) (Syntax.Block body)) = codegen $ Syntax.While cond (Syntax.Block $ body ++ inc)
codegen (Syntax.Call fname fargs) = do
  args <- mapM (\a -> do
    arg <- codegen a
    return (arg, [])) fargs
  state <- get'
  case (getTypeByName state (AST.Name fname)) of
    Just funcType -> call (ref (funcType) (AST.Name fname)) args
    Nothing       -> error $ "could not find type of function " ++ show fname
codegen (Syntax.BinOp op lhs rhs) = case Map.lookup op binops of
  Just fn -> do
    lhs' <- codegen lhs
    rhs' <- codegen rhs
    fn lhs' rhs'
  Nothing -> error $ "no such operator: " ++ (show op)

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
  buildFunction "main" [] (inferType insts) insts
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
