import           Test.Hspec
import           Prelude
import           System.IO
import           System.Process
import           System.Exit
import           System.Posix.Files
import qualified LLVM.AST              as AST

import qualified Compiler
import qualified Syntax
import qualified Testcases

exec :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
exec cmd = do
  createProcess (proc "bash" ["-c", escapedCmd]){ std_out = CreatePipe }
  where escapedCmd = '"' : cmd ++ "\"" -- FIXME the command isn't really escaped. But we doesn't really need it.

testcasePath :: String -> String -> String
testcasePath folder path = "./test/testcases/" ++ folder ++ ('/':path)

testCompilationAndOutput :: String -> String -> IO ()
testCompilationAndOutput path expected = do
  Compiler.runCompiler $ testcasePath "kaleidoscope" path
  (_, hout, _, _) <- exec "./a.out"
  case hout of
    Nothing   -> do putStrLn "An error occurred..."; exitFailure
    Just hout -> do
      output <- hGetContents hout
      output `shouldBe` expected

testCodeGeneration :: [Syntax.Expr] -> String -> IO ()
testCodeGeneration es expectedPath = do
  asm <- Compiler.compile es
  expected <- readFile $ testcasePath "asm" expectedPath
  asm `shouldBe` expected

testParsing :: String -> [Syntax.Expr] -> IO ()
testParsing path expected = undefined

main :: IO ()
main = hspec $ do
  describe "Code generation" $ do
    it "generate a function returning a float" $ do
      testCodeGeneration Testcases.float42 "42.asm"
    it "generate a function returning the last expression value" $ do
      testCodeGeneration Testcases.floatsEndingWith42 "42.asm"
  describe "Compilation and output" $ do
    it "prints Hello, World!" $ do
      testCompilationAndOutput "hello_world.kk" "Hello, World!\n"
