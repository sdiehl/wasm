module Language.Wasm.Entry(
  main,
  parse,
) where

import Language.Wasm.Core (toCore)
{-import Language.Wasm.Hex-}
import Language.Wasm.Lexer
import Language.Wasm.Monad
import Language.Wasm.Parser
import Language.Wasm.Pretty ()
import Language.Wasm.Syntax
{-import Language.Wasm.Eval-}
import Language.Wasm.Binary ()
{-import Language.Wasm.Verify-}

import qualified Data.ByteString as BS
import Data.Serialize
import System.Environment
import System.Process
import Text.PrettyPrint.ANSI.Leijen
import qualified Hexdump

-- import           Text.Show.Pretty

parse :: String -> Either ParseError [Decl]
parse fs = runParseM prog (scan fs)

file :: FilePath -> IO (Either ParseError [Decl])
file fname = parse `fmap` readFile fname

main :: IO ()
main = do
  args <- getArgs
  let input = case args of
                 [input] -> input
                 _       -> "example1.wasm"

  ast1 <- file input
  putStrLn $ show ast1

  case ast1 of
    Left err -> return ()
    Right [mod] -> do
      {-putStrLn $ "=== AST ==="-}
      putDoc $ pretty mod
      putStrLn $ "=== WAST ==="
      let bs = encode (toCore mod)
      putStrLn $ "=== HEX ==="
      putStrLn $ Hexdump.prettyHex bs
      return ()
    Right _ -> return ()

  putStrLn "Done"
  return ()
