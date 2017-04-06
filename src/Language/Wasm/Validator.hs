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
