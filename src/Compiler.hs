{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecursiveDo #-}

module Compiler (preprocess, compile, runCompiler, jit, hasArg) where

import           Control.Monad.Except
import           Foreign.Ptr (FunPtr, castFunPtr)
import           Foreign.C.String
import           Data.List
import           Data.Maybe
import           System.Environment
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

import           Codegen
import qualified Syntax

import           Debug.Trace

foreign import ccall "dynamic" haskFunInt :: FunPtr (IO Int) -> (IO Int)
foreign import ccall "dynamic" haskFunDouble :: FunPtr (IO Double) -> (IO Double)
foreign import ccall "dynamic" haskFunCString :: FunPtr (IO CString) -> (IO CString)

compiler = "llc"
linker = "gcc"
optimizationPasses = defaultCuratedPassSetSpec { optLevel = Just 3 }

hasArg :: String -> IO Bool
hasArg arg = do
  args <- getArgs
  return $ isJust $ find (\a -> a == arg) args

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
      -- runPassManager pm compiledModule
      asm <- moduleLLVMAssembly compiledModule
      return $ BS.unpack asm

jit :: [Syntax.Expr] -> IO ()
jit expr = withContext $ \context ->
  jitCompiler context $ \executionEngine ->
    withModuleFromAST context (preprocess expr) $ \compiledModule ->
      withPassManager optimizationPasses $ \pm -> do
        -- runPassManager pm compiledModule
        p <- hasArg "--asm"
        case p of
          True -> do
            asm <- moduleLLVMAssembly compiledModule
            putStrLn "======= ASM ======="
            putStrLn $ BS.unpack asm
            putStrLn "===================\n"
          _ -> pure ()
        EE.withModuleInEngine executionEngine compiledModule $ \ee -> do
          mainfn <- EE.getFunction ee (AST.Name "main")
          case mainfn of
            Just fn -> do
              putStr "< "
              runCEntrypoint (inferType $ last $ expr) fn
            Nothing -> return ()
        return ()
  where
    runCEntrypoint (IntegerType 32) fn = do
      res <- haskFunInt (castFunPtr fn :: FunPtr (IO Int))
      putStrLn $ show res
    runCEntrypoint (FloatingPointType DoubleFP) fn = do
      res <- haskFunDouble (castFunPtr fn :: FunPtr (IO Double))
      putStrLn $ show res
    runCEntrypoint (PointerType (IntegerType 8) _) fn = do
      res <- haskFunCString (castFunPtr fn :: FunPtr (IO CString))
      str <- peekCString res
      putStrLn str

runCompiler :: String -> IO ()
runCompiler filename = do
  content <- readFile filename
  case Syntax.parse content of
    Left ((i, j), err) -> putStrLn $ "error: " ++ show err
    Right exprs -> do
      p <- hasArg "--asm"
      asm <- Compiler.compile $ exprs
      case p of
        True -> do
          putStrLn "======= ASM ======="
          putStrLn asm
          putStrLn "===================\n"
        _ -> pure ()
      writeFile (filename ++ ".ll") asm
      command_ [] compiler [filename ++ ".ll"]
      command_ [] linker [filename ++ ".s"]
      -- command_ [] "rm" [filename ++ ".ll", filename ++ ".s"]
