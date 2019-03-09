module Main (main) where

import           Data.List
import           Data.Maybe
import           Data.String
import           Data.String.Encode
import           System.IO
import           System.Environment
import           System.Exit
import           System.Command
import qualified LLVM.Module
import qualified LLVM.AST.Type as LLVM

import qualified Syntax
import qualified Compiler
import qualified Interpreter

import           Debug.Trace

getFilename :: IO (Maybe String)
getFilename = do
  args <- getArgs
  return $ find (\a -> (head a) /= '-') args

main :: IO ()
main = do
  file <- getFilename
  case file of
    Just f -> Compiler.runCompiler f
    Nothing -> Interpreter.runInterpreter []

abort :: String -> IO ()
abort message = do
  putStrLn $ "koak: error: " ++ message
  exitWith $ ExitFailure 84
