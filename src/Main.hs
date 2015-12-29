module Main where

import Lexer
import Parser
import Monad

import Control.Monad

foo :: String -> Either ParseError Token
foo fs = runParseM prog (scan fs)

main :: IO ()
main = return ()
