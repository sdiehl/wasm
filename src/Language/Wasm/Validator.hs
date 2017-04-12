module Language.Wasm.Validator where

import Control.Applicative.Lift

import qualified Data.Sequence as S
import qualified Data.Text as T
import Language.Wasm.Core

data Mutability = Immutable | Mutable deriving (Eq, Show)

data GlobalType = GlobalType Type Mutability deriving (Eq, Show)

data ExternType
  = ExternFuncType
  | ExternTableType
  | ExternMemoryType
  | ExternGlobalType GlobalType

data Context = Context
  { mod     :: Module
  , globals :: S.Seq GlobalType
  , locals  :: S.Seq Type
  , results :: S.Seq Type
  , labels  :: S.Seq [Type]
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

type ValidatorM = Errors [ValidationError]

validate :: Context -> Expr -> ValidatorM Type
validate ctx e = case e of
              Nop               ->  pure (Arrow [] Void)
              Unreachable       ->  todo
              (Block x xs)      ->  todo
              (If x y)          ->  todo
              (IfElse x y z)    ->  todo
              (BrIf x y z)      ->  todo
              (Loop x y xs)     ->  todo
              (Br x y)          ->  todo
              (Return x)        ->  todo
              (Call x xs)       ->  todo
              (Const x y)       ->  todo
              (Lit x)           ->  todo
              (Load x y)        ->  todo
              (Store x y)       ->  todo
              (GetLocal i)      ->  case (S.lookup i (locals ctx)) of
                                      (Just t) -> pure (Arrow [] t)
                                      Nothing  -> failure []
              (SetLocal x y)    ->  todo
              (LoadExtend x y)  ->  todo
              (StoreWrap x y z) ->  todo
              (Bin x y z w)     ->  todo
              (Un x y z)        ->  todo
              (Rel x y z w)     ->  todo
              (Sel x y z w)     ->  todo
              (Convert x y z)   ->  todo
              (Host x xs)       ->  todo


runValidateM :: Context -> Expr -> Either [ValidationError] Type
runValidateM ctx e = runErrors (validate ctx e)


todo = failure [NotImplemented]
