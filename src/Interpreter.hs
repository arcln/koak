module Interpreter (runInterpreter) where

import           System.IO
import           Control.Exception

import qualified Syntax
import qualified Compiler

runInterpreter :: [Syntax.Expr] -> IO ()
runInterpreter mod = do
  putStr "> "
  hFlush stdout
  eof <- isEOF
  if eof then pure () else do
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
        result <- try (Compiler.jit $ (previousFuncs mod) ++ exprs) :: IO (Either SomeException ())
        case result of
          Left e -> do
            putStrLn $ "error: " ++ (show e)
            runInterpreter $ previousFuncs mod
          _ -> runInterpreter $ (previousFuncs mod) ++ exprs
  where
    previousFuncs b = [fn f | f <- b, isPersistent f]
    isPersistent (Syntax.Function {}) = True
    isPersistent (Syntax.Extern {}) = True
    isPersistent (Syntax.Decl {}) = True
    isPersistent (Syntax.Block {}) = True
    isPersistent _ = False
    fn (Syntax.Block b) = Syntax.Block $ previousFuncs b
    fn x = x