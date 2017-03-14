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
parser inputs = testGroup "Parser does not fail on syntactically correct inputs" $ do
  -- list comp
  input <- inputs
  return $ testCase (takeBaseName (fst input)) ( assertBool "" (isRight (parse (snd input))) )

prettyPrinter :: TestTree
prettyPrinter = testGroup "Pretty Printer" []

binaryWriter :: TestTree
binaryWriter = testGroup "Binary Writer" []



main :: IO ()
main = do
  paths <- testFiles
  contents <- mapM readFile paths
  defaultMain $ testGroup "Test Suite" [ parser (zip paths contents), prettyPrinter, binaryWriter]
