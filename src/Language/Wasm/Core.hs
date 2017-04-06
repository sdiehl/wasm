{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE MultiParamTypeClasses  #-}

module Language.Wasm.Core (
  toCore,
  Func(..),
  Decl(..),
  Module(..),
  Import(..),
  Export(..),

  -- reexport common elements
  Syn.Name(..),
  Syn.Expr(..),
  Syn.Type(..),
  Syn.Value(..),
  {-Syn.Param(..),-}
  Syn.UnOp(..),
  Syn.BinOp(..),
  Syn.RelOp(..),
  Syn.ConvertOp(..)
) where

import qualified Language.Wasm.Syntax as Syn

{-class ToCore a b | a -> b where-}
  {-toCore :: a -> b-}

{-instance ToCore Syn.Module Module where-}
  {-toCore = undefined-}

data Decl
  = ModDecl Module
  | ExprDecl Syn.Expr
  deriving (Eq, Show)

data Module = Module
  { _funcs   :: [Func]
  , _imports :: [Import]
  , _exports :: [Export]
  } deriving (Eq, Show)

data Func = Func
  { _fname  :: Maybe Syn.Name
  , _params :: Int
  , _fbody  :: [Syn.Expr]
  } deriving (Eq, Show)

data Import
  = Import Syn.Name Syn.Name
  deriving (Eq, Show)

data Export
  = Export Syn.Name Syn.Name
  deriving (Eq, Show)

-------------------------------------------------------------------------------
-- Conversion
-------------------------------------------------------------------------------

toCoreFunc :: Syn.Func -> Func
toCoreFunc (Syn.Func ftype params _) = Func ftype (length params) body
  where
    body = [f | Syn.Body f <- params]
    result = head [f | Syn.Result f <- params]

toCoreExport :: Syn.Func -> Export
toCoreExport (Syn.Export fname val) = Export (undefined fname) (undefined fname)

toCoreImport :: Syn.Func -> Import
toCoreImport _ = undefined

toCoreMod :: Syn.Module -> Module
toCoreMod (Syn.Module defs) = Module
  { _funcs   = [toCoreFunc f | f@Syn.Func {} <- defs]
  , _imports = [toCoreImport f | f@Syn.Import {} <- defs]
  , _exports = [toCoreExport f | f@Syn.Export {} <- defs]
  }

toCoreDecl :: Syn.Decl -> Decl
toCoreDecl (Syn.ModDecl mod)  = ModDecl (toCoreMod mod)
toCoreDecl (Syn.ExprDecl exp) = ExprDecl exp

toCore = toCoreDecl

{-toCore :: [Syn.Decl] -> [Decl]-}
{-toCore = fmap toCoreDecl-}
