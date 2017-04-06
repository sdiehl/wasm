{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Language.Wasm.Syntax (
  Name(..),
  Expr(..),
  Decl(..),
  Func(..),
  Type(..),
  Value(..),
  Param(..),
  Module(..),
  UnOp(..),
  BinOp(..),
  RelOp(..),
  ConvertOp(..),
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
  | Bin BinOp Type Expr Expr
  | Un UnOp Type Expr
  | Rel RelOp Type Expr Expr
  | Sel SelOp Expr Expr Expr
  | Convert ConvertOp Type Expr
  | Host Hostop [Expr]
  deriving (Eq, Show)

data UnOp
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

data BinOp
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
  | RotR
  | RotL
  | Div
  | CopySign
  | Min
  | Max

  deriving (Eq, Show)

data SelOp
  = Select
  deriving (Eq, Show)

data RelOp
  = Eqz
  | Eq
  | Ne
  | LtS
  | LtU
  | GtS
  | GtU
  | LeS
  | LeU
  | GeS
  | GeU
  | Lt
  | Gt
  | Le
  | Ge
  deriving (Eq, Show)

data ConvertOp
  = WrapI64
  | TruncSF32
  | TruncUF32
  | TruncSF64
  | TruncUF64
  | ExtendSI32
  | ExtendUI32
  | ConvertSI32
  | ConvertUI32
  | ConvertSI64
  | ConvertUI64
  | DemoteF64
  | PromoteF32
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
  { ty     :: Value
  , offset :: Offset
  , align  :: Maybe Int
  } deriving (Eq, Show)

data Hostop
  = MemorySize
  | GrowMemory
  | HasFeature Name
  deriving (Eq, Show)

data Value
  = VI32 Int32
  | VI64 Int64
  | VF32 Float
  | VF64 Double
  deriving (Eq, Show)

data Decl
  = ModDecl Module
  | ExprDecl Expr
  deriving (Eq, Show)

data Func = Func
  { _ftype  :: Maybe Name
  , _params :: [Param]
  , _body   :: [Expr]
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
