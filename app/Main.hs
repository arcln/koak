module Main (main, test) where

import           System.IO
import           System.Environment
import           System.Exit
import           System.Command

import qualified Syntax
import qualified Compiler
import qualified LLVM.Module

import Debug.Trace

runInterpreter :: [Syntax.Expr] -> IO ()
runInterpreter mod = do
  putStr "> "
  hFlush stdout
  input <- getLine
  let finput = if last input /= ';' then input ++ ";" else input
  case Syntax.parse finput of
    Left ((i, j), err) -> putStrLn err
    -- Right exprs -> trace (show exprs) $ do
    Right exprs -> do
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

start :: [String] -> IO ()
start [] = runInterpreter []
start args = Compiler.runCompiler $ head args

main :: IO ()
main = start =<< getArgs

abort :: String -> IO ()
abort message = do
  putStrLn $ "koak: error: " ++ message
  exitWith $ ExitFailure 84

test :: String -> IO ()
test file = do
  start [file]
  command_ [] "./a.out" []
