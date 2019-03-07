module Main (main) where

import           Data.List
import           Data.Maybe
import           System.IO
import           System.Environment
import           System.Exit
import           System.Command

import qualified Syntax
import qualified Compiler
import qualified LLVM.Module

import Debug.Trace

getFilename :: IO (Maybe String)
getFilename = do
  args <- getArgs
  return $ find (\a -> (head a) /= '-') args

runInterpreter :: [Syntax.Expr] -> IO ()
runInterpreter mod = do
  putStr "koak> "
  hFlush stdout
  input <- getLine
  let finput = if last input /= ';' then input ++ ";" else input
  case Syntax.parse finput of
    Left ((i, j), err) -> putStrLn err
    Right exprs -> do
      p <- Compiler.hasArg "--ast"
      case p of
        True -> do
          putStrLn "======= AST ======="
          putStrLn (show exprs)
          putStrLn "===================\n"
        _ -> pure ()
      Compiler.jit $ (previousFuncs mod) ++ exprs
      runInterpreter $ (previousFuncs mod) ++ exprs
  where
    previousFuncs b = [fn f | f <- b, isPersistent f]
    isPersistent (Syntax.Function {}) = True
    isPersistent (Syntax.Extern {}) = True
    isPersistent (Syntax.Decl {}) = True
    isPersistent (Syntax.Block {}) = True
    isPersistent _ = False
    fn (Syntax.Block b) = Syntax.Block $ previousFuncs b
    fn x = x

main :: IO ()
main = do
  file <- getFilename
  case file of
    Just f -> Compiler.runCompiler f
    Nothing -> runInterpreter []

abort :: String -> IO ()
abort message = do
  putStrLn $ "koak: error: " ++ message
  exitWith $ ExitFailure 84
