{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler (preprocess, compile, runCompiler, jit) where

import           Control.Monad.Except
import           Foreign.Ptr (FunPtr, castFunPtr)
import qualified Data.ByteString.Char8 as BS

import           LLVM.Module
import           LLVM.Context
import           LLVM.PassManager
import           LLVM.Transforms
import           LLVM.Analysis
import           LLVM.IRBuilder
import           LLVM.AST.Type
import qualified LLVM.AST              as AST
import qualified LLVM.AST.Constant     as C
import qualified LLVM.ExecutionEngine  as EE
import           System.Command

import Debug.Trace


import           Codegen
import qualified Syntax

foreign import ccall "dynamic" haskFun :: FunPtr (IO Int) -> (IO Int)

compiler = "llc"
linker = "gcc"
optimizationPasses = defaultCuratedPassSetSpec { optLevel = Just 3 }

preprocess :: [Syntax.Expr] -> AST.Module
preprocess expr = buildModule "main" $ startCodegen expr []

jitCompiler :: Context -> (EE.MCJIT -> IO a) -> IO a
jitCompiler c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing

compile :: [Syntax.Expr] -> IO String
compile expr = withContext $ \context ->
  withModuleFromAST context (preprocess expr) $ \compiledModule ->
    withPassManager optimizationPasses $ \pm -> do
      runPassManager pm compiledModule
      asm <- moduleLLVMAssembly compiledModule
      return $ BS.unpack asm

jit :: [Syntax.Expr] -> IO ()
jit expr = withContext $ \context ->
  jitCompiler context $ \executionEngine ->
    withModuleFromAST context (preprocess expr) $ \compiledModule ->
      withPassManager optimizationPasses $ \pm -> do
        -- runPassManager pm compiledModule
        asm <- moduleLLVMAssembly compiledModule
        putStrLn $ BS.unpack asm
        putStrLn "==================="
        EE.withModuleInEngine executionEngine compiledModule $ \ee -> do
          mainfn <- EE.getFunction ee (AST.Name "main")
          case mainfn of
            Just fn -> do
              res <- runCEntrypoint fn
              putStrLn $ show res
            Nothing -> return ()
        return ()
  where runCEntrypoint fn = haskFun (castFunPtr fn :: FunPtr (IO Int))

runCompiler :: String -> IO ()
runCompiler filename = do
  content <- readFile filename
  case Syntax.parse content of
    Left ((i, j), err) -> putStrLn $ "error: " ++ show err
    Right exprs -> trace ("exprs:" ++ show exprs) $ do
      asm <- Compiler.compile $ exprs
      writeFile (filename ++ ".ll") asm
      command_ [] compiler [filename ++ ".ll"]
      command_ [] linker [filename ++ ".s"]
      -- command_ [] "rm" [filename ++ ".ll", filename ++ ".s"]