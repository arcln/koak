{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler (compile, simple) where

import LLVM.Module
import LLVM.Context
import LLVM.IRBuilder

import qualified LLVM.AST as AST
import qualified LLVM.AST.Type as Type
import qualified LLVM.AST.Constant as Constant
import qualified Data.ByteString.Char8 as BS
import qualified Syntax

simple :: AST.Module
simple = buildModule "exampleModule" $ mdo
  function "add" [(Type.i32, "a"), (Type.i32, "b")] Type.i32 $ \[a, b] -> mdo
    entry <- block `named` "entry"; do
      c <- add a b
      ret c

genMain :: AST.Module
genMain = buildModule "exampleModule" $ mdo
  function "main" [] Type.i32 $ \[] -> mdo
    entry <- block `named` "entry"; do
      ret $ AST.ConstantOperand $ Constant.Int 32 0

codegen :: Syntax.Expr -> AST.Module
codegen _ = genMain

compile :: Syntax.Expr -> IO String
compile expr = postProcess $ codegen expr
  where
    postProcess m = withContext $ \context ->
      withModuleFromAST context m $ \compiledModule -> do
        llstr <- moduleLLVMAssembly compiledModule
        return $ BS.unpack llstr

-- type SymbolTable = [(String, AST.Operand)]

-- type Argument = (AST.Type, AST.Name)

-- newtype Compiler a = Compiler (State AST.Module a)
--   deriving (Functor, Applicative, Monad, MonadState AST.Module)

-- compile :: AST.Module -> Compiler a -> AST.Module
-- compile m (Compiler c) = execState c m

-- define :: String -> AST.Type -> [Argument] ->

-- createModule :: String -> AST.Module
-- createModule name = defaultModule { moduleName = label }
