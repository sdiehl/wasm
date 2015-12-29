module Main where

import Lexer
import Parser
import Monad
import Syntax
import Pretty
import Eval
import Verify

import Control.Monad
import Control.Applicative

parse :: String -> Either ParseError [Decl]
parse fs = runParseM prog (scan fs)

file :: FilePath -> IO (Either ParseError [Decl])
file fname = do
  contents <- readFile fname
  {-print $ scan contents-}
  return $ parse contents

main :: IO ()
main = do
  ast1 <- file "example1.wasm"
  ast2 <- file "example2.wasm"
  print ast1
  print ast2
  putStrLn "Done"
  return ()
