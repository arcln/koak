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
exec cmd = createProcess (proc "bash" ["-c", cmd]){ std_out = CreatePipe }

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

testKoak :: String -> String -> IO ()
testKoak path expected = do
  (_, hout, _, _) <- exec $ "stack exec koak-exe -- " ++ (testcasePath "kaleidoscope" path)
  case hout of
    Nothing   -> do putStrLn "An error occurred..."; exitFailure
    Just hout -> do
      output <- hGetContents hout
      output `shouldBe` expected

testJit :: String -> String -> String -> IO ()
testJit input args expected = do
  (_, hout, _, _) <- exec $ "echo -e '" ++ input ++ "' | stack exec koak-exe -- " ++ args
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
  describe "Boilerplate" $ do
    -- it "works" $ testCompilationAndOutput "error.kk" ""
    it "don't crash on LLVM error" $ "crash" `shouldBe` "works"

  describe "Parsing" $ do
    it "parses 42 as int" $ do
      testParsing "42.kk" Testcases.int42
    it "parses 42.0 as double" $ do
      testParsing "42.0.kk" Testcases.double42
    it "parses a function with no arguments returning an int" $ do
      testParsing "no_arguments_42.kk" Testcases.fnNoArgs42
    it "parses a function with no arguments returning a double" $ do
      testParsing "no_arguments_42.0.kk" Testcases.fnNoArgs42_0

  describe "Code generation" $ do
    it "generates a main function returning a double" $ do
      testCodeGeneration Testcases.double42 "42.0.asm"
    it "generates a main function returning the last expression value" $ do
      testCodeGeneration Testcases.floatsEndingWith42 "42.asm"
    it "generates a function taking no arguments and returning a double" $ do
      testCodeGeneration Testcases.fnNoArgs42_0 "no_arguments_42.0.asm"
    it "generates two functions with one calling the other and returning an int" $ do
      testCodeGeneration Testcases.fnCallNoArgs42 "fn_call_42.asm"
    it "generates two functions with one calling the other and returning a double" $ do
      testCodeGeneration Testcases.fnCallNoArgs42_0 "fn_call_42.0.asm"
    it "generates two functions with one calling the other with an argument" $ do
      testCodeGeneration Testcases.fnCallDoubleArg "42.0.asm" -- could not work, depending on compiler optimisations
    it "generates a function taking no arguments and returning a double" $ do
      testCodeGeneration Testcases.fnNoArgs42_0 "no_arguments_42.0.asm"

  describe "Compiler" $ do
    it "displays the assembly code with the --asm switch" $ do
      testKoak "hello_world.kk --asm" "======= ASM =======\n; ModuleID = 'main'\nsource_filename = \"<string>\"\n\n@0 = unnamed_addr constant [14 x i8] c\"Hello, world!\\00\"\n\ndeclare i32 @puts(i8*)\n\ndefine i32 @main() {\nentry:\n  %0 = bitcast [14 x i8]* @0 to i8*\n  %1 = call i32 @puts(i8* %0)\n  ret i32 %1\n}\n\n===================\n\n"
    it "displays the parser AST with the --ast switch" $ do
      testKoak "hello_world.kk --ast" "======= AST =======\n[Extern \"puts\" [PointerType {pointerReferent = IntegerType {typeBits = 8}, pointerAddrSpace = AddrSpace 0}] (IntegerType {typeBits = 32}) False,Block [Call \"puts\" [Data (Str \"Hello, world!\")]]]\n===================\n\n"
    it "displays AST and ASM with both --ast and --asm switches" $ do
      testKoak "hello_world.kk --ast" "======= AST =======\n[Extern \"puts\" [PointerType {pointerReferent = IntegerType {typeBits = 8}, pointerAddrSpace = AddrSpace 0}] (IntegerType {typeBits = 32}) False,Block [Call \"puts\" [Data (Str \"Hello, world!\")]]]\n===================\n\n"

  describe "JIT interpreter" $ do
    it "launches without arguments and exits with no error code" $ do
      testJit "" "" "> "
    it "launches without arguments and return an int" $ do
      testJit "42" "" "> < 42\n> "
    it "launches without arguments and return a double" $ do
      testJit "42.0" "" "> < 42.0\n> "
    it "launches without arguments and return a string" $ do
      testJit "\"aze\"" "" "> < aze\n> "
    it "launches with --ast switch" $ do
      testJit "42" "--ast" "> ======= AST =======\n[Block [Data (Int 42)]]\n===================\n\n< 42\n> "
    it "launches with --asm switch" $ do
      testJit "42" "--asm" "> ======= ASM =======\n; ModuleID = 'main'\nsource_filename = \"<string>\"\n\ndefine i32 @main() {\nentry:\n  ret i32 42\n}\n\n===================\n\n< 42\n> "
    it "launches with both --asm and --ast switches" $ do
      testJit "42" "--asm --ast" "> ======= AST =======\n[Block [Data (Int 42)]]\n===================\n\n======= ASM =======\n; ModuleID = 'main'\nsource_filename = \"<string>\"\n\ndefine i32 @main() {\nentry:\n  ret i32 42\n}\n\n===================\n\n< 42\n> "
    it "executes multiples instructions" $ do
      testJit "42\nusing puts(string): int; puts(\"aze\");" "" "> < 42\n> < 10\n> aze\n"

  describe "Koak" $ do
    it "prints Hello, World!" $ do
      testCompilationAndOutput "hello_world.kk" "Hello, world!\n"
    it "declares and prints a 'foo' variable" $ do
      testCompilationAndOutput "var.kk" "42\n"
    it "declares, reassign and prints a 'foo' variable" $ do
      testCompilationAndOutput "assign.kk" "42\n"
    it "outputs an error when variable name not found" $ do
      testCompilationAndOutput "var_error.kk" ""
    it "outputs an error when assignin an unknown variable" $ do
      testCompilationAndOutput "assign_error.kk" ""
    it "handles conditionnal branchings using if keyword" $ do
      testCompilationAndOutput "if.kk" "yes\nno\nyes\n"
    it "prints numbers from 0 to 9 using a while loop" $ do
      testCompilationAndOutput "while.kk" "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
    it "prints numbers from 0 to 9 using a for loop" $ do
      testCompilationAndOutput "for.kk" "0\n1\n2\n3\n4\n5\n6\n7\n8\n9\n"
    it "correctly handles comparison operators" $ do
      testCompilationAndOutput "comp_op.kk" "yes\nno\nno\nyes\nyes\nno\nno\nyes\nyes\nno\nyes\nno\nyes\nyes\n"
    it "correctly handles comparison operators on doubles" $ do
      -- testCompilationAndOutput "comp_op_double.kk" "yes\nno\nno\nyes\nyes\nno\nno\nyes\nyes\nno\nyes\nno\nyes\nyes\n"
      "crash" `shouldBe` "works"
    it "correctly handles computing operators" $ do
      testCompilationAndOutput "calc_op.kk" "2\n10000002\n4\n0\n-9999998\n5\n0\n0\n0\n0\n100\n50\n2\n5\n0\n0\n"
    it "correctly handles computing operators on doubles" $ do
    --   testCompilationAndOutput "calc_op_double.kk" "yes\nno\nno\nyes\nyes\nno\nno\nyes\nyes\nno\nyes\nno\nyes\nyes\n"
      "crash" `shouldBe` "works"
    it "correctly handles operators priority" $ do
      testCompilationAndOutput "prio_op.kk" "2\n10000008\n4\n16\n5\n1\n-9\n8\n1\n2\n"
    it "auto infer bit type to integer type if needed on func call" $ do
      testCompilationAndOutput "cast_bit_to_int.kk" "1\n"
    it "auto infer int type to double type if needed on func call" $ do
      testCompilationAndOutput "cast_int_to_double.kk" "42\n"
    it "handle multi-type computing with operators" $ do
      testCompilationAndOutput "multi_type_compute.kk" "1764.000000\n1764.000000\n42\n42\n42.000000\n42.000000\n1764.000000\n1764.000000\n1764.000000\n1764.000000\n1764.000000\n1764.000000\n"
