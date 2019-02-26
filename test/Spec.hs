import           Test.Hspec
import           Prelude
import           System.IO
import           System.Process
import           System.Exit
import           System.Posix.Files
import qualified Compiler

exec :: String -> IO (Maybe Handle, Maybe Handle, Maybe Handle, ProcessHandle)
exec cmd = do
  createProcess (proc "bash" ["-c", escapedCmd]){ std_out = CreatePipe }
  where escapedCmd = '"' : cmd ++ "\""

compileAndCheckOutput path expected = do
  -- let binPath = "./kaleidobin"
  -- code <- readFile $ "./test/testcases/" ++ path
  Compiler.runCompiler $ "./test/testcases/" ++ path
  -- writeFile binPath code
  -- setFileMode binPath $ foldl unionFileModes ownerReadMode [ownerWriteMode, ownerExecuteMode]

  (_, hout, _, _) <- exec "./a.out"
  case hout of
    Nothing   -> do putStrLn "An error occurred..."; exitFailure
    Just hout -> do
      output <- hGetContents hout
      output `shouldBe` expected

main :: IO ()
main = hspec $ do
  describe "Hello world" $ do
    it "prints Hello, World!" $ do
      compileAndCheckOutput "hello_world.kk" "Hello, World!\n"

