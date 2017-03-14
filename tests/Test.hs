module Main (
  main
) where
import Control.Monad.Trans
import Data.Either
import Language.Wasm.Entry (parse)
import System.FilePath
import Test.Tasty
import Test.Tasty.Golden
import Test.Tasty.HUnit

testFiles :: MonadIO m => m [FilePath]
testFiles = liftIO $ findByExtension [".wast"] "tests/spec/"




parser :: [(FilePath, String)] -> TestTree
parser inputs = testGroup "Parser does not fail on syntactically correct inputs" testCases
  where
    testCases = [ testCase (takeBaseName name) (assertion contents) | (name, contents) <- inputs ]
    assertion contents = let result = parse contents
                         in assertBool (show result) (isRight (parse contents))



prettyPrinter :: TestTree
prettyPrinter = testGroup "Pretty Printer" []

binaryWriter :: TestTree
binaryWriter = testGroup "Binary Writer" []



main :: IO ()
main = do
  paths <- testFiles
  contents <- mapM readFile paths
  defaultMain $ testGroup "Test Suite" [ parser (zip paths contents), prettyPrinter, binaryWriter]
