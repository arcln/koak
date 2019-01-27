module Main (main, test) where

import System.Environment
import System.Exit
import System.Command

import qualified Syntax
import qualified Compiler
import qualified LLVM.Module

compiler = "llc-7.0"
linker = "gcc"

parse :: String -> Syntax.Expr
parse input = Syntax.Float 0

run :: String -> IO String
run input = Compiler.compile $ parse input

start :: [String] -> IO ()
start [] = abort "no input files"
start args = do
  content <- readFile file
  asm <- run content
  writeFile "__tmp__.ll" asm
  command_ [] compiler ["__tmp__.ll"]
  command_ [] linker ["__tmp__.s"]
  command_ [] "rm" ["__tmp__.ll", "__tmp__.s"]
  where file = head args

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
