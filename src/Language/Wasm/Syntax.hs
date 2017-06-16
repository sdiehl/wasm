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
  | Load MemOp Expr
  | Store MemOp Expr
  | GetLocal Name
  | SetLocal Name Expr
  | LoadExtend ExtOp Expr
  | StoreWrap WrapOp Expr Expr
  | Bin BinOp Type Expr Expr
  | Un UnOp Type Expr
  | Rel RelOp Type Expr Expr
  | Sel SelOp Expr Expr Expr
  | Convert ConvertOp Type Expr
  | Host HostOp [Expr]
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

  | ReinterpretF32
  | ReinterpretF64
  | ReinterpretI32
  | ReinterpretI64
  deriving (Eq, Show)

data ExtOp
  = ExtOp MemOp MemSize Extension
  deriving (Eq, Show)

data WrapOp
  = WrapOp MemOp MemSize
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

data MemOp = MemOp
  { ty     :: Value
  , offset :: Offset
  , align  :: Maybe Int
  }  deriving (Eq, Show)

data HostOp
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

newtype Module = Module
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
  | Arrow [Type] Type
  deriving (Eq, Show)
