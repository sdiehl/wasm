{

module Parser where

import Monad
import Lexer
import Syntax

import qualified Data.Text as T

}

%name prog prog
%tokentype { Token }
%monad { ParseM }
%lexer { lexerP } { TEnd }

%token

ident              { TIdent $$ }
integer            { TNat $$ }
char               { TChar $$ }
str                { TString $$ }
real               { TReal $$ }

'('                { TKey "(" }
')'                { TKey ")" }
'.'                { TKey "." }

'i32'              { TKey "i32" }
'i64'              { TKey "i64" }
'f32'              { TKey "f32" }
'f64'              { TKey "f64" }
'void'             { TKey "void" }

'module'           { TKey "module" }
'export'           { TKey "export" }
'func'             { TKey "func" }
'const'            { TKey "const" }
'param'            { TKey "param" }
'local'            { TKey "local" }
'result'           { TKey "result" }
'class'            { TKey "class" }
'block'            { TKey "block" }

'loop'             { TKey "loop" }
'return'           { TKey "return" }
'call'             { TKey "call" }
'call_import'      { TKey "call_impport" }
'call_indirect'    { TKey "call_indirect" }
'if_else'          { TKey "if_else" }
'if'               { TKey "if" }
'br'               { TKey "br" }
'br_if'            { TKey "br_if" }
'tableswitch'      { TKey "tableswitch" }
'case'             { TKey "case" }

'get_local'        { TKey "get_local" }
'set_local'        { TKey "set_local" }

'add'              { TKey "add" }
'mul'              { TKey "mul" }
'sub'              { TKey "sub" }
'div'              { TKey "div" }
'abs'              { TKey "abs" }
'eq'               { TKey "eq" }
'min'              { TKey "min" }
'max'              { TKey "max" }
'ceil'             { TKey "ceil" }
'trunc'            { TKey "trunc" }
'floor'            { TKey "floor" }
'neg'              { TKey "neg" }
'sqrt'             { TKey "sqrt" }
'nearest'          { TKey "nearest" }
'copysign'         { TKey "copysign" }

'nop'              { TKey "nop" }
'unreachable'      { TKey "nop" }


%left 'and' 'or' 'xor'
%left '<' '>' '<=' '>=' '=' '/='
%left '+' '-'
%left '*' '/' '//'

%%

-- Syntax

prog :: { [Decl] }
 : list(top)                      { $1 }

name :: { Name }
 : ident                          { Name (T.pack $1) }
 | integer                        { Name (T.pack (show $1)) }

top :: { Decl }
 : mod                            { ModDecl $1 }
 | sexp                           { ExprDecl $1 }

mod :: { Module }
 : '(' 'module' list(func) ')'    { Module $3 }

typ :: { Type }
 : 'i32'                           { I32 }
 | 'i64'                           { I64 }
 | 'f32'                           { F32 }
 | 'f64'                           { F64 }
 | 'void'                          { Void }

param :: { Param }
 : '(' 'param' name typ ')'       { Param (Just $3) $4 }
 | '(' 'param' typ ')'            { Param Nothing $3 }
 | '(' 'result' typ ')'           { Result $3 }
 | sexp                           { Body $1 }

func :: { Func }
 : '(' 'func' name list1(param) ')' 
 { Func (Just $3) $4 [] }

 | '(' 'func' list1(param) ')' 
 { Func Nothing $3 [] }

 | '(' 'export' str value ')'     { Export $3 $4 }

sexp :: { Expr }
 : '(' expr ')'                   { $2 }
 | value                          { Lit $1 }

value :: { Value }
 : real                           { VFloat $1 }
 | integer                        { VInt $1 }

binop :: { Binop }
 : 'mul'                          { Mul }
 | 'add'                          { Add }
 | 'sub'                          { Sub }
 | 'div'                          { Div }
 | 'min'                          { Min }
 | 'max'                          { Max }
 | 'copysign'                     { CopySign }

unop :: { Unop }
 : 'neg'                          { Neg }
 | 'abs'                          { Abs }
 | 'ceil'                         { Ceil }
 | 'floor'                        { Floor }
 | 'trunc'                        { Trunc }
 | 'nearest'                      { Nearest }
 | 'sqrt'                         { Sqrt }

relop :: { Relop }
 : 'eq'                           { Eq }

expr :: { Expr }
 : 'nop'                          { Nop }
 | 'unreachable'                  { Unreachable }
 | 'block' list(sexp)             { Block Nothing $2 }
 | 'block' name list(sexp)        { Block (Just $2) $3 }
 | 'if' sexp sexp                 { If $2 $3 }
 | 'if_else' sexp sexp sexp       { IfElse $2 $3 $4 }
 | 'br_if' sexp name sexp         { BrIf $2 $3 $4 }

 | 'loop' name name list(sexp)    { Loop (Just $2) (Just $3) $4 }
 | 'loop' name list(sexp)         { Loop (Just $2) Nothing $3 }
 | 'loop' list(sexp)              { Loop Nothing Nothing $2 }

 | 'br' name sexp                 { Br $2 (Just $3) }
 | 'br' name                      { Br $2 Nothing }
 | 'return' sexp                  { Return $2 }
 | 'call' name list(sexp)         { Call $2 $3 }
 | 'get_local' name               { GetLocal $2 }
 | 'set_local' name sexp          { SetLocal $2 $3 }
 | typ '.' binop sexp sexp        { Bin $3 $1 $4 $5 }
 | typ '.' unop sexp              { Un $3 $1 $4 }
 | typ '.' 'const' value          { Const $1 $4 }
 | typ '.' relop sexp sexp        { Rel $3 $1 $4 $5 }

-- Utils

rev_list(p)
  : rev_list(p) p  { $2 : $1 }
  | {- empty -}    { [] }

rev_list1(p)
  : rev_list1(p) p { $2 : $1 }
  | p              { [$1] }

-- List of zero or more p.
list(p)
  : rev_list(p)    { reverse $1 }

-- A list of at least 1 p's
list1(p) : rev_list1(p)   { reverse $1 }


{

happyError :: ParseM a
happyError = ParseM $ \(S _ toks) -> Left $ ParseError $ "happyError: " ++ show toks

}
