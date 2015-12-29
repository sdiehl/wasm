{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Syntax (
  Name(..),
  Expr(..),
  Decl(..),
  Func(..),
  Type(..),
  Value(..),
  Param(..),
  Module(..),
) where

import Data.String
import qualified Data.Text as T

newtype Name = Name T.Text
  deriving (Eq, Ord, Show, IsString)

data Expr
  = Nop
  | Unreachable
  | Block (Maybe Name) [Expr]
  | If Expr Expr
  | IfElse Expr Expr Expr
  | BrIf Expr Name Expr
  | Loop (Maybe Name) (Maybe Name) [Expr]
  | Br Name (Maybe Expr)
  | Return Expr
  | Call Name [Expr]
  | GetLocal Name
  | SetLocal Name Expr
  | Const Type Value
  | Mul Type Expr Expr
  | Sub Type Expr Expr
  | Lit Value
  deriving (Eq, Show)

data Value
  = VInt Integer
  | VFloat Double
  deriving (Eq, Show)

data Decl
  = ModDecl Module
  | ExprDecl Expr
  deriving (Eq, Show)

data Func = Func
  { _ftype :: Maybe Name
  , _params :: [Param]
  , _body :: [Expr]
  }
  deriving (Eq, Show)

data Module = Module
  { _funcs   :: [Func]
  , _imports :: [Import]
  , _exports :: [Export]
  }
  deriving (Eq, Show)

data Param
  = Param (Maybe Name) Type
  | Result Type
  | Body Expr
  deriving (Eq, Show)

data Import
  = Import Name Name
  deriving (Eq, Show)

data Export
  = Export Name Name
  deriving (Eq, Show)

data Type
  = I32
  | I64
  | F32
  | F64
  deriving (Eq, Show)
