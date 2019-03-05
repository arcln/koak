module LLVM (extern', function') where

import Data.Traversable
import LLVM.AST hiding (function')
import LLVM.AST.Global
import LLVM.AST.Type
import LLVM.AST.ParameterAttribute
import LLVM.AST.Constant
import LLVM.AST.Linkage
import LLVM.IRBuilder
import LLVM.IRBuilder.Module

fst' (x, _, _) = x
thd (_, _, x) = x

function' :: MonadModuleBuilder m => Name -> [(Type, ParameterName, [ParameterAttribute])] -> Type -> ([Operand] -> IRBuilderT m ()) -> m Operand
function' label argtys retty body = do
  (paramNames, blocks) <- runIRBuilderT emptyIRBuilder $ do
    paramNames <- forM argtys $ \(_, paramName, _) -> case paramName of
      NoParameterName -> fresh
      ParameterName p -> fresh `named` p
    body $ zipWith LocalReference (map fst' argtys) paramNames
    return paramNames
  let
    def = GlobalDefinition functionDefaults
      { name        = label
      , parameters  = (zipWith (\tyfl nm -> Parameter (fst' tyfl) nm (thd tyfl)) argtys paramNames, False)
      , returnType  = retty
      , basicBlocks = blocks
      }
    funty = ptr $ FunctionType retty (fst' <$> argtys) False
  emitDefn def
  pure $ ConstantOperand $ GlobalReference funty label

extern' :: MonadModuleBuilder m => Name -> [Type] -> Type -> m Operand
extern' nm argtys retty = do
  emitDefn $ GlobalDefinition functionDefaults
    { name       = nm
    , linkage    = External
    , parameters = ([Parameter ty (mkName (show id)) [] | (ty, id) <- zip argtys [0..(length argtys - 1)]], False)
    , returnType = retty
    }
  let funty = ptr $ FunctionType retty argtys False
  pure $ ConstantOperand $ GlobalReference funty nm