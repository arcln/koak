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
testParsing path expected = do
  code <- readFile $ testcasePath "kaleidoscope" path
  case (Syntax.parse code) of
    Left e -> [Testcases.parseError e] `shouldBe` expected
    Right ast     -> ast `shouldBe` expected

main :: IO ()
main = hspec $ do
  describe "Parsing" $ do
    it "parse 42 as int" $ do
      testParsing "42.kk" Testcases.int42
    it "parse 42.0 as double" $ do
      testParsing "42.0.kk" Testcases.double42
    it "parse a function with no arguments returning an int" $ do
      testParsing "no_arguments_42.kk" Testcases.fnNoArgs42
    it "parse a function with no arguments returning a double" $ do
      testParsing "no_arguments_42.0.kk" Testcases.fnNoArgs42_0

  describe "Code generation" $ do
    it "generate a main function returning a double" $ do
      testCodeGeneration Testcases.double42 "42.0.asm"
    it "generate a main function returning the last expression value" $ do
      testCodeGeneration Testcases.floatsEndingWith42 "42.asm"
    it "generate a function taking no arguments and returning a double" $ do
      testCodeGeneration Testcases.fnNoArgs42_0 "no_arguments_42.0.asm"
    it "generate two functions with one calling the other and returning an int" $ do
      testCodeGeneration Testcases.fnCallNoArgs42 "fn_call_42.asm"
    it "generate two functions with one calling the other and returning a double" $ do
      testCodeGeneration Testcases.fnCallNoArgs42_0 "fn_call_42.0.asm"
    it "generate two functions with one calling the other with an argument" $ do
      testCodeGeneration Testcases.fnCallDoubleArg "42.0.asm" -- could not work, depending on compiler optimisations

  describe "Compilation and output" $ do
    it "prints Hello, World!" $ do
      testCompilationAndOutput "hello_world.kk" "Hello, World!\n" -- FIXME hello_world.kk is to be written
