module Main (main, test) where

import System.Environment
import System.Exit
import           System.Command

import qualified Syntax
import qualified Compiler
import qualified LLVM.Module

runInterpreter :: IO ()
runInterpreter = do
  input <- getLine
  Compiler.jit $ Syntax.parse input
  runInterpreter

start :: [String] -> IO ()
start [] = runInterpreter
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
