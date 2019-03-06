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
  case Syntax.parse input of
    Left ((i, j), err) -> putStrLn err
    -- Right exprs -> trace (show exprs) $ do
    Right exprs -> do
      Compiler.jit $ previousFuncs ++ exprs
      runInterpreter $ previousFuncs ++ exprs
  where
    previousFuncs = [f | f <- mod, isPersistent f]
    isPersistent (Syntax.Function {}) = True
    isPersistent (Syntax.Extern {}) = True
    isPersistent _ = False

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
