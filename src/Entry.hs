module Entry(
  main,
) where

import           Binary
import           Core                         (toCore)
import           Eval
import           Hex
import           Lexer
import           Monad
import           Parser
import           Pretty
import           Syntax
import           Verify

import           Control.Applicative
import           Control.Monad

import qualified Data.ByteString              as BS
import           Data.Char
import           Data.Serialize
import           Data.Word
import           System.Environment
import           System.Process
import           Text.PrettyPrint.ANSI.Leijen

-- import           Text.Show.Pretty

parse :: String -> Either ParseError [Decl]
parse fs = runParseM prog (scan fs)

file :: FilePath -> IO (Either ParseError [Decl])
file fname = do
  contents <- readFile fname
  {-print $ scan contents-}
  return $ parse contents

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
      putDoc $ pretty mod
      let bs = encode (toCore mod)
      {-mapM_ print (ByteString.unpack bs)-}
      {-fd <- open "example1.bin"-}
      putStrLn $ simpleHex bs
      BS.writeFile "example1.bin" bs
      system "hexdump example1.bin"
      return ()

  putStrLn "Done"
  return ()
