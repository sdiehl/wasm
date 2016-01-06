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

{-
and expr' =
  | Nop                                     (* do nothing *)
  | Unreachable                             (* trap *)
  | Block of expr list                      (* execute in sequence *)
  | Loop of expr                            (* loop header *)
  | Break of var * expr option              (* break to n-th surrounding label *)
  | If of expr * expr * expr                (* conditional *)
  | Switch of expr * var list * var * expr list   (* table switch *)
  | Call of var * expr list                 (* call function *)
  | CallImport of var * expr list           (* call imported function *)
  | CallIndirect of var * expr * expr list  (* call function through table *)
  | GetLocal of var                         (* read local variable *)
  | SetLocal of var * expr                  (* write local variable *)
  | Load of memop * expr                    (* read memory at address *)
  | Store of memop * expr * expr            (* write memory at address *)
  | LoadExtend of extop * expr              (* read memory at address and extend *)
  | StoreWrap of wrapop * expr * expr       (* wrap and write to memory at address *)
  | Const of literal                        (* constant *)
  | Unary of unop * expr                    (* unary arithmetic operator *)
  | Binary of binop * expr * expr           (* binary arithmetic operator *)
  | Select of selop * expr * expr * expr    (* branchless conditional *)
  | Compare of relop * expr * expr          (* arithmetic comparison *)
  | Convert of cvtop * expr                 (* conversion *)
  | Host of hostop * expr list              (* host interaction *)
-}

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
  { _funcs   :: [Func]
  , _imports :: [Func]
  , _exports :: [Func]
  }
  deriving (Eq, Show)

data Param
  = Param (Maybe Name) Type
  | Result Type
  | Body Expr
  deriving (Eq, Show)

{-data Import-}
  {-= Import Name Name-}
  {-deriving (Eq, Show)-}

{-data Export-}
  {-= Export Name Name-}
  {-deriving (Eq, Show)-}

data Type
  = Void
  | I32
  | I64
  | F32
  | F64
  | All
  deriving (Eq, Show)
