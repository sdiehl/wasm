module Language.Wasm.Validator where
import Control.Applicative.Lift
import Language.Wasm.Core

data Mutability = Immutable | Mutable deriving (Eq, Show)

data GlobalType = GlobalType Type Mutability deriving (Eq, Show)

data Context = Context
  { mod     :: Module
  , globals :: [GlobalType]
  , locals  :: [Type]
  , results :: [Type]
  , labels  :: [[Type]]
  } deriving (Eq, Show)

data ValidationError
  = DuplicatedExportName
  | MutableGlobalImportedOrExported
  | InvalidConversion { to :: Type, from :: Type }
  | ConstantExpressionRequired
  | TypeMismatch { expected :: Type, given :: Type }
  | MultipleTablesNotSupported
  | MultipleMemoriesNotSupported
  | StartFunctionMustNotHaveParamsOrResults
  | NotImplemented

type ValidatorM = Errors ValidationError

validate :: Context -> Expr -> ValidatorM Type
validate ctx Nop               = todo
validate ctx Unreachable       = todo
validate ctx (Block y xs)      = todo
validate ctx (If y z)          = todo
validate ctx (IfElse y z w)    = todo
validate ctx (BrIf y z w)      = todo
validate ctx (Loop y z xs)     = todo
validate ctx (Br y z)          = todo
validate ctx (Return y)        = todo
validate ctx (Call y xs)       = todo
validate ctx (Const y z)       = todo
validate ctx (Lit y)           = todo
validate ctx (Load y z)        = todo
validate ctx (Store y z)       = todo
validate ctx (GetLocal y)      = todo
validate ctx (SetLocal y z)    = todo
validate ctx (LoadExtend y z)  = todo
validate ctx (StoreWrap y z w) = todo
validate ctx (Bin y z w s)     = todo
validate ctx (Un y z w)        = todo
validate ctx (Rel y z w s)     = todo
validate ctx (Sel y z w s)     = todo
validate ctx (Convert y z w)   = todo
validate ctx (Host y xs)       = todo


todo = failure NotImplemented
