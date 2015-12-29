{

module Parser where

import Monad
import Lexer

}

%name prog prog
%tokentype { Token }
%monad { ParseM }
%lexer { lexerP } { TEnd }

%token
  IDENTIFIER         { TIdent $$ }
  INTEGER            { TNat $$ }
  CHARACTER_CONSTANT { TChar $$ }
  MANIFEST_STRING    { TString $$ }
  REAL               { TReal $$ }

  'class'            { TKey "class" }


%left 'and' 'or' 'xor'
%left '<' '>' '<=' '>=' '=' '/='
%left '+' '-'
%left '*' '/' '//'

%%

prog :: { Token }
 : 'class'                    { $1 }


{

happyError :: ParseM a
happyError = ParseM $ \(S _ toks) -> Left $ ParseError $ "happyError: " ++ show toks

}
