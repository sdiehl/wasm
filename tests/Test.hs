module Main (
  main
) where

import Control.Monad.Trans
import Data.Either
import Data.Serialize

import Language.Wasm.Binary
import Language.Wasm.Core
import Language.Wasm.Pretty
import Language.Wasm.Entry (parse)

import System.FilePath
import System.FilePath.Posix

import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.Golden

import qualified Hexdump
import qualified Data.Text.Lazy.IO as T
import qualified Data.ByteString as BS

testFiles :: MonadIO m => m [FilePath]
testFiles = liftIO $ findByExtension [".wast"] "tests/spec/input"

parser :: [(FilePath, String)] -> TestTree
parser inputs = testGroup "Parser does not fail on syntactically correct inputs" testCases
  where
    testCases = [ testCase (takeBaseName name) (assertion contents) | (name, contents) <- inputs ]
    assertion contents = let result = parse contents
                         in assertBool (show result) (isRight result)
prettyPrinter :: TestTree
prettyPrinter = testGroup "Pretty Printer" []

binaryWriter :: [(FilePath, String)] -> TestTree
binaryWriter inputs = testGroup "Binary Writer" $ do
  (path, contents) <- inputs
  let
    outf    = "tests/spec/output" </> replaceExtension (takeFileName path) "wasm"
    astf    = "tests/spec/output" </> replaceExtension (takeFileName path) "ast"
    goldenf = "tests/spec/golden" </> replaceExtension (takeFileName path) "wasm"
    action = do
      let ast = (parse contents)
      case ast of
        Left err -> return ()
        Right [mod] -> do
          let bs = encode (toCore mod)
          writeFile astf (display mod)
          writeFile outf (Hexdump.prettyHex bs)
  return $ goldenVsFile (takeBaseName path) goldenf outf action

main :: IO ()
main = do
  paths <- testFiles
  contents <- mapM readFile paths
  let files = (zip paths contents)
  defaultMain $ testGroup "Test Suite" [ parser files, prettyPrinter, binaryWriter files]
