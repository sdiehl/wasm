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
  Unop(..),
  Binop(..),
  Relop(..),
) where

import Data.Int
import Data.String
import qualified Data.Text as T

newtype Name = Name T.Text
  deriving (Eq, Ord, Show, IsString)

data Expr
  = Nop
  | Unreachable
  | Block (Maybe Name) [Expr]
  | Break Name (Maybe Expr)
  | If Expr Expr
  | IfElse Expr Expr Expr
  | BrIf Expr Name Expr
  | Loop (Maybe Name) (Maybe Name) [Expr]
  | Br Name (Maybe Expr)
  | Return Expr
  | Call Name [Expr]
  | Const Type Value
  | Lit Value
  | Load Memop Expr
  | Store Memop Expr
  | GetLocal Name
  | SetLocal Name Expr
  | LoadExtend Extop Expr
  | StoreWrap Wrapop Expr Expr
  | Bin Binop Type Expr Expr
  | Un Unop Type Expr
  | Rel Relop Type Expr Expr
  | Sel Selop Expr Expr Expr
  | Convert Cvtop Expr
  | Host Hostop [Expr]
  deriving (Eq, Show)

data Unop
  -- Integer
  = Clz
  | Ctz
  | Popcnt

  -- Floating Point
  | Neg
  | Abs
  | Ceil
  | Floor
  | Trunc
  | Nearest
  | Sqrt
  deriving (Eq, Show)

data Binop
  -- Integer
  = Add
  | Sub
  | Mul
  | DivS
  | DivU
  | RemS
  | RemU
  | And
  | Or
  | Xor
  | Shl
  | ShrU
  | ShrS

  -- Floating Point
  | Div
  | CopySign
  | Min
  | Max
  deriving (Eq, Show)

data Selop
  = Select
  deriving (Eq, Show)

data Relop
  -- Integer
  = Eq
  | Ne
  | LtS
  | LtU
  | LeS
  | LeU
  | GtS
  | GtU
  | GeS
  | GeU

  -- Floating Point
  | Lt
  | Le
  | Gt
  | Ge
  deriving (Eq, Show)

data Cvtop
  = ExtendSInt32
  | ExtendUInt32
  | WrapInt64
  | TruncSFloat32
  | TruncUFloat32
  | TruncSFloat64
  | TruncUFloat64
  | ReinterpretFloat
  | ConvertSInt32
  | ConvertUInt32
  | ConvertSInt64
  | ConvertUInt64
  | PromoteFloat32
  | DemoteFloat64
  | ReinterpretInt
  deriving (Eq, Show)

data Extop
  = Extop Memop MemSize Extension
  deriving (Eq, Show)

data Wrapop
  = Wrapop Memop MemSize
  deriving (Eq, Show)

type Address = Int64
type Size = Int64
type Offset = Address

data MemSize
  = Mem8
  | Mem16
  | Mem32
  deriving (Eq, Show)

data Extension
  = SX
  | ZX
  deriving (Eq, Show)

data Memop = Memop
  { ty :: Value
  , offset :: Offset
  , align :: Maybe Int
  } deriving (Eq, Show)

data Hostop
  = MemorySize
  | GrowMemory
  | HasFeature Name
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
  | Export String Value
  | Import Name Int
  deriving (Eq, Show)

data Module = Module
  { _funcs :: [Func]
  } deriving (Eq, Show)

data Param
  = Param (Maybe Name) Type
  | Result Type
  | Body Expr
  deriving (Eq, Show)


data Type
  = Void
  | I32
  | I64
  | F32
  | F64
  | All
  deriving (Eq, Show)
