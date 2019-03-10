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
import           Control.Exception
import           System.Command

import           Codegen
import           LLVM
import           Helpers
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

preprocess :: [Syntax.Expr] -> (Maybe Type, AST.Module)
preprocess expr = buildModule' "main" $ startCodegen expr ([], expr)

jitCompiler :: Context -> (EE.MCJIT -> IO a) -> IO a
jitCompiler c = EE.withMCJIT c optlevel model ptrelim fastins
  where
    optlevel = Just 2
    model    = Nothing
    ptrelim  = Nothing
    fastins  = Nothing

-- runJIT :: AST.Module -> IO (Either String AST.Module)
-- runJIT mod = do
--   withContext $ \context ->
--     runExceptT $ withModuleFromAST context mod $ \m ->
--       withPassManager passes $ \pm -> do
--         runPassManager pm m
--         optmod <- moduleAST m
--         s <- moduleLLVMAssembly m
--         putStrLn s
--         return optmod

removeCallStack :: String -> String
removeCallStack s = case findIndex (\c -> c == '\n') s of
  Just idx -> take idx s
  Nothing -> s

compile :: [Syntax.Expr] -> IO (Either String String)
compile expr = withContext $ \context -> do
  let (_, ast) = preprocess expr
  result <- try (withModuleFromAST context ast $ \compiledModule ->
    withPassManager optimizationPasses $ \pm -> do
      p <- hasArg "-O"
      case p of
        True -> runPassManager pm compiledModule
        _ -> pure False
      asm <- moduleLLVMAssembly compiledModule
      return $ BS.unpack asm) :: IO (Either SomeException String)
  case result of
    Left (SomeException e) -> return $ Left $ "error: " ++ (removeCallStack $ show e)
    Right asm -> return $ Right asm


jit :: [Syntax.Expr] -> IO ()
jit expr = withContext $ \context ->
  jitCompiler context $ \executionEngine -> do
    let (retType, ast) = preprocess expr
    result <- try (withModuleFromAST context ast $ \compiledModule ->
      withPassManager optimizationPasses $ \pm -> do
        p' <- hasArg "-O"
        case p' of
          True -> runPassManager pm compiledModule
          _ -> pure False
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
              runCEntrypoint retType fn
            Nothing -> return ()
        return ()) :: IO (Either SomeException ())
    case result of
      Left (SomeException e) -> putStrLn $ "error: " ++ (show e)
      _ -> pure ()
  where
    runCEntrypoint (Just (IntegerType 32)) fn = do
      res <- haskFunInt (castFunPtr fn :: FunPtr (IO Int))
      putStrLn $ show res
    runCEntrypoint (Just (FloatingPointType DoubleFP)) fn = do
      res <- haskFunDouble (castFunPtr fn :: FunPtr (IO Double))
      putStrLn $ show res
    runCEntrypoint (Just (PointerType (IntegerType 8) _)) fn = do
      res <- haskFunCString (castFunPtr fn :: FunPtr (IO CString))
      str <- peekCString res
      putStrLn str
    runCEntrypoint Nothing fn = pure ()
    runCEntrypoint (Just t) fn = error $ "received type " ++ (show t)

runCompiler :: String -> IO ()
runCompiler filename = do
  content <- readFile filename
  case Syntax.parse content of
    Left err -> printError err
    Right exprs -> do
      showAsm <- hasArg "--asm"
      showAst <- Compiler.hasArg "--ast"
      case showAst of
        True -> do
          putStrLn "======= AST ======="
          putStrLn (show exprs)
          putStrLn "===================\n"
        _ -> pure ()
      asm <- Compiler.compile $ exprs
      case (asm, showAsm) of
        (Right asm', True) -> do
          putStrLn "======= ASM ======="
          putStrLn asm'
          putStrLn "===================\n"
        _ -> pure ()
      case asm of
        Right asm' -> do
          writeFile (filename ++ ".ll") asm'
          command_ [] compiler [filename ++ ".ll"]
          command_ [] linker [filename ++ ".s"]
          command_ [] "rm" ["-f", filename ++ ".ll", filename ++ ".s"]
        Left e -> putStrLn e
