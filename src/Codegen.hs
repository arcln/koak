{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Codegen (startCodegen, inferType, toBS, ) where

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

op = AST.ConstantOperand
ref t n = op $ C.GlobalReference t n
local t n = AST.LocalReference t (AST.Name n)

as :: AST.Operand -> Type -> IRBuilderT ModuleBuilder AST.Operand
as o@(AST.ConstantOperand (C.Int 1 _)) (IntegerType 1) = pure o
as o@(AST.ConstantOperand (C.Int 1 _)) (IntegerType 32) = zext o int
as o@(AST.ConstantOperand (C.Int 32 _)) (IntegerType 1) = trunc o bool
as o@(AST.ConstantOperand (C.Int 32 _)) (IntegerType 32) = pure o
as o@(AST.ConstantOperand (C.Int 32 _)) (FloatingPointType DoubleFP) = sitofp o Types.double
as o@(AST.ConstantOperand (C.Float _)) (IntegerType 1) = fptosi o bool
as o@(AST.ConstantOperand (C.Float _)) (IntegerType 32) = fptosi o int
as o@(AST.ConstantOperand (C.Float _)) (FloatingPointType DoubleFP) = pure o
as o@(AST.LocalReference (IntegerType 1) _) (IntegerType 1) = pure o
as o@(AST.LocalReference (IntegerType 1) _) (IntegerType 32) = zext o int
as o@(AST.LocalReference (IntegerType 32) _) (IntegerType 1) = trunc o bool
as o@(AST.LocalReference (IntegerType 32) _) (IntegerType 32) = pure o
as o@(AST.LocalReference (IntegerType 32) _) (FloatingPointType DoubleFP) = sitofp o Types.double
as o@(AST.LocalReference (FloatingPointType DoubleFP) _) (IntegerType 1) = fptosi o bool
as o@(AST.LocalReference (FloatingPointType DoubleFP) _) (IntegerType 32) = fptosi o int
as o@(AST.LocalReference (FloatingPointType DoubleFP) _) (FloatingPointType DoubleFP) = pure o
as o@(AST.LocalReference (IntegerType 1) _) (FloatingPointType DoubleFP) = do
  tmp <- zext o int
  sitofp tmp Types.double
as o@(AST.ConstantOperand (C.Int 1 _)) (FloatingPointType DoubleFP) = do
  tmp <- zext o int
  sitofp tmp Types.double
as o@(AST.LocalReference t _) t'
  | t == t' = pure o
  | otherwise = error $ "cannot cast " ++ (show t) ++ " to " ++ (show t')
as o t = error $ "cannot cast " ++ (show o) ++ " to " ++ (show t)

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

getMB :: ModuleBuilder ModuleBuilderState
getMB = liftModuleState $ get

toBS :: AST.Name -> BS.ShortByteString
toBS (AST.Name n) = n
toBS (AST.UnName n) = BS.toShort $ C8.pack $ drop 1 $ show n

zipVaArgs :: [a] -> [b] -> [(a, Maybe b)]
zipVaArgs as bs = zipVaArgs' as bs []
  where
    zipVaArgs' [] _ out = out
    zipVaArgs' (a:as) [] out = zipVaArgs' as [] $ out ++ [(a, Nothing)]
    zipVaArgs' (a:as) (b:bs) out = zipVaArgs' as bs $ out ++ [(a, Just b)]

getFuncDefByName :: ModuleBuilderState -> AST.Name -> Maybe AST.Definition
getFuncDefByName (ModuleBuilderState builderDefs _) name = find byName (getSnocList builderDefs)
  where
    byName (AST.GlobalDefinition (AST.Function _ _ _ _ _ _ typeName _ _ _ _ _ _ _ _ _ _)) = typeName == name
    byName _ = False

getFnRetType :: AST.Definition -> AST.Type
getFnRetType (AST.GlobalDefinition (AST.Function _ _ _ _ _ funcType _ _ _ _ _ _ _ _ _ _ _)) = funcType

getFnArgsType :: AST.Definition -> [AST.Type]
getFnArgsType (AST.GlobalDefinition (AST.Function _ _ _ _ _ _ _ params _ _ _ _ _ _ _ _ _)) = paramsType
  where paramsType = map (\(Parameter t _ _) -> t) $ fst params

getFuncType :: AST.Definition -> AST.Type
getFuncType (AST.GlobalDefinition (AST.Function _ _ _ _ _ funcType _ (params, isVaArgs) _ _ _ _ _ _ _ _ _)) = asPointer $ AST.FunctionType funcType paramsTypes isVaArgs
  where
    paramsTypes = map (\(AST.Parameter t _ _) -> t) params
    asPointer fn = PointerType (fn) (A.AddrSpace 0)

getVarTypeByName :: ModuleBuilderState -> AST.Name -> AST.Type
getVarTypeByName (ModuleBuilderState _ builderTypeDefs) name = case Map.lookup name builderTypeDefs of
  Just t  -> t
  Nothing -> error $ "could not find type of variable " ++ show name

getFnTypeByName :: ModuleBuilderState -> AST.Name -> AST.Type
getFnTypeByName mbs name = case getFuncDefByName mbs name of
  Just func -> getFuncType func
  Nothing   -> error $ "could not find type of function " ++ show name

getFnArgsTypeByName :: ModuleBuilderState -> AST.Name -> [AST.Type]
getFnArgsTypeByName mbs name = case getFuncDefByName mbs name of
  Just func -> getFnArgsType func
  Nothing   -> error $ "could not find args type of function " ++ show name

getFnRetTypeByName :: ModuleBuilderState -> AST.Name -> AST.Type
getFnRetTypeByName mbs name = case getFuncDefByName mbs name of
  Just func -> getFnRetType func
  Nothing   -> error $ "could not find return type of function " ++ show name

getVarTypeByName' :: [Syntax.Expr] -> AST.Name -> AST.Type
getVarTypeByName' ast name = case astFind' declByName ast of
  Just (Syntax.Decl t _ _) -> t
  Nothing -> error $ "could not find type of variable " ++ show name ++ " in the ast"
  where
    declByName d@(Syntax.Decl _ dname _ ) = (AST.Name dname) == name
    declByName _                          = False

getFnRetTypeByName' :: [Syntax.Expr] -> AST.Name -> AST.Type
getFnRetTypeByName' ast name = case astFind' funcByName ast of
  Just (Syntax.Function _ _ t _)  -> t
  Just (Syntax.Extern _ _ t _)    -> t
  Nothing -> error $ "could not find type of function " ++ show name ++ " in the ast"
  where
    funcByName d@(Syntax.Function fname _ _ _ ) = (AST.Name fname) == name
    funcByName d@(Syntax.Extern ename _ _ _ )   = (AST.Name ename) == name
    funcByName _                                = False

astFind' :: (Syntax.Expr -> Bool) -> [Syntax.Expr] -> Maybe Syntax.Expr
astFind' cond exprs = case catMaybes $ map (astFind cond) exprs of
  (x:_) -> Just x
  []    -> Nothing

astFind :: (Syntax.Expr -> Bool) -> Syntax.Expr -> Maybe Syntax.Expr
astFind cond b@(Syntax.Block exprs)             = astFind' cond exprs
astFind cond d@(Syntax.Decl _ _ expr)           = if cond d then Just d else Nothing
astFind cond f@(Syntax.Function _ exprs _ expr) = if cond f then Just f else astFind' cond (expr:exprs)
astFind cond e@(Syntax.Extern _ _ _ _)          = if cond e then Just e else Nothing
astFind cond i@(Syntax.If c thenb elseb)        = astFind' cond [c, thenb, elseb]
astFind cond w@(Syntax.While a b)               = astFind' cond [a, b]
astFind cond f@(Syntax.For a b c d)             = astFind' cond [a, b, c, d]
astFind cond _ = Nothing
-- astFind cond a@(Syntax.Assign _ expr)     = astFind cond expr
-- astFind cond v@(Syntax.Var _)             = if cond v then Just v else Nothing
-- astFind cond c@(Syntax.Call _ exprs)      = astFind' cond exprs
-- astFind cond b@(Syntax.BinOp _ lhs rhs)   = astFind' cond [lhs, rhs]

inferTypes :: ModuleBuilderState -> String -> Syntax.Expr -> Syntax.Expr -> (Type, Type)
inferTypes s name lhs rhs
  | lhsType == Types.double || rhsType == Types.double = (Types.double, inferRetType Types.double name)
  | lhsType == int || rhsType == int                   = (int, inferRetType int name)
  | lhsType == bool || rhsType == bool                 = (int, inferRetType int name)
  | otherwise = error $ name ++ " types mismatch " ++ (show lhsType) ++ " " ++ (show rhsType)
  where
    lhsType = inferType s lhs
    rhsType = inferType s rhs
    inferRetType _ "Eq"    = bool
    inferRetType _ "NotEq" = bool
    inferRetType _ "Lt"    = bool
    inferRetType _ "Gt"    = bool
    inferRetType _ "Lte"   = bool
    inferRetType _ "Gte"   = bool
    inferRetType d _       = d

-- <<<<<<< develop
-- inferType s (Syntax.Block []) = int
-- inferType s (Syntax.Block b) = last $ map (inferType s) b
-- inferType s (Syntax.Data (Syntax.Double _)) = Types.double
-- inferType s (Syntax.Data (Syntax.Int _)) = int
-- inferType s (Syntax.Data (Syntax.Str _)) = charptr
-- inferType s (Syntax.Decl t _ _) = t
-- inferType s (Syntax.Assign _ expr) = inferType s expr
-- inferType s (Syntax.Var name) = getVarTypeByName s (AST.Name name)
-- inferType s (Syntax.If _ thenb elseb) = fst $ inferTypes s "if-else" thenb elseb
-- inferType s (Syntax.While _ b) = inferType s b
-- inferType s (Syntax.For _ _ _ b) = inferType s b
-- inferType s (Syntax.Call fname _) = getFnRetTypeByName s (AST.Name fname)
-- inferType s (Syntax.BinOp op lhs rhs) = fst $ inferTypes s (show op) lhs rhs

getStrongType :: ModuleBuilderState -> Syntax.Expr -> Type
getStrongType s (Syntax.BinOp op lhs rhs) = snd $ inferTypes s (show op) lhs rhs
getStrongType s e = inferType s e

inferType :: ModuleBuilderState -> Syntax.Expr -> Type
inferType _ (Syntax.Block [])               = int
inferType s (Syntax.Block b)                = last $ map (inferType s) b
inferType _ (Syntax.Data (Syntax.Double _)) = Types.double
inferType _ (Syntax.Data (Syntax.Int _))    = int
inferType _ (Syntax.Data (Syntax.Str _))    = charptr
inferType _ (Syntax.Decl t _ _)             = t
inferType s (Syntax.Assign _ expr)          = inferType s expr
inferType s (Syntax.Var name)               = getVarTypeByName s (AST.Name name)
inferType s (Syntax.If _ thenb elseb)       = fst $ inferTypes s "if-else" thenb elseb
inferType s (Syntax.While _ b)              = inferType s b
inferType s (Syntax.For _ _ _ b)            = inferType s b
inferType s (Syntax.Call fname _)           = getFnRetTypeByName s (AST.Name fname)
inferType s (Syntax.BinOp op lhs rhs)       = fst $ inferTypes s (show op) lhs rhs

inferType' :: [Syntax.Expr] -> Syntax.Expr -> Type
inferType' _   (Syntax.Block [])               = int
inferType' ast (Syntax.Block b)                = last $ map (inferType' ast) b
inferType' _   (Syntax.Data (Syntax.Double _)) = Types.double
inferType' _   (Syntax.Data (Syntax.Int _))    = int
inferType' _   (Syntax.Data (Syntax.Str _))    = charptr
inferType' _   (Syntax.Decl t _ _)             = t
inferType' ast (Syntax.Assign _ expr)          = inferType' ast expr
inferType' ast (Syntax.Var name)               = getVarTypeByName' ast (AST.Name name)
-- inferType' ast (Syntax.If _ thenb elseb)       = inferTypes ast "if-else" thenb elseb
inferType' ast (Syntax.While _ b)              = inferType' ast b
inferType' ast (Syntax.For _ _ _ b)            = inferType' ast b
inferType' ast (Syntax.Call fname _)           = getFnRetTypeByName' ast (AST.Name fname)
-- inferType' ast (Syntax.BinOp op lhs rhs)       = inferTypes ast (show op) lhs rhs


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

  condv' <- codegen cond
  condv  <- trunc condv' bool
  condBr condv tname fname

  emitBlockStart tname
  tout <- codegen thenb
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
  state <- getIR
  let funcArgs = getFnArgsTypeByName state (AST.Name fname)
  let funcRet = getFnRetTypeByName state (AST.Name fname)
  let funcType = getFnTypeByName state (AST.Name fname)
  args <- mapM genArg $ zipVaArgs fargs funcArgs
  call (ref (funcType) (AST.Name fname)) args
    where
      genArg (a, t) = do
        arg <- case t of
          Just t' -> codegen a >>= (`as` t')
          _ -> codegen a
        return (arg, [])
codegen e@(Syntax.BinOp op lhs rhs) = do
  state <- getIR
  let opType = inferType state e
  let retType = getStrongType state e
  let binops = if opType == int then ibinops else fbinops
  case Map.lookup op binops of
    Just fn -> do
      lhs'  <- codegen lhs
      lhs'' <- lhs' `as` opType
      rhs'  <- codegen rhs
      rhs'' <- rhs' `as` opType
      res   <- fn lhs'' rhs''
      case retType of
        IntegerType 1 -> res `as` int
        _ -> res `as` retType
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

startCodegen :: [Syntax.Expr] -> ([Syntax.Expr], [Syntax.Expr]) -> ModuleBuilder (Maybe Type)
startCodegen [] ([], _)       = return Nothing
startCodegen [] (mainEs, ast) = do
  let retType = inferType' ast insts
  buildFunction "main" [] retType insts
  return $ Just retType
    where insts = Syntax.Block $ reverse mainEs
startCodegen (Syntax.Function name args retType body:es) state = do
  buildFunction name args retType body
  startCodegen es state
startCodegen (Syntax.Extern name argsType retType True:es) state = do
  externVarArgs (AST.Name name) argsType retType
  startCodegen es state
startCodegen (Syntax.Extern name argsType retType False:es) state = do
  extern' (AST.Name name) argsType retType
  startCodegen es state
startCodegen (expr:es) (mainEs, ast) = startCodegen es (expr:mainEs, ast)

