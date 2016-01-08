{-# OPTIONS_GHC -fdefer-type-errors #-}
{-# LANGUAGE FlexibleInstances #-}

module Binary where

import Data.Word
import Data.ByteString
import Data.Serialize
import qualified Data.List as List

import Syntax

-------------------------------------------------------------------------------
-- Binary Writer
-------------------------------------------------------------------------------

data WasmSectionType
  = SectionMemory
  | SectionSignatures
  | SectionFunction
  | SectionGlobals
  | SectionDataSegments
  | SectionFunctionTable
  | SectionEnd
  deriving (Eq, Show)

instance Serialize WasmSectionType where
  put x = case x of
    SectionMemory        -> putWord8 0
    SectionSignatures    -> putWord8 1
    SectionFunction      -> putWord8 2
    SectionGlobals       -> putWord8 3
    SectionDataSegments  -> putWord8 4
    SectionFunctionTable -> putWord8 5
    SectionEnd           -> putWord8 6
  get = error "get Type"

instance Serialize Type where
  put t = case t of
    Void -> putWord8 0
    I32  -> putWord8 1
    I64  -> putWord8 2
    F32  -> putWord8 4
    F64  -> putWord8 8
    All  -> putWord8 15
  get = error "get Type"

instance Serialize Decl where
  put (ModDecl mod) = do
    put mod
  put (ExprDecl mod) = do
    undefined
  get = error "get Type"

instance Serialize Func where
  put (Export name _) = do
    putWord32le 15
    put name
  put (Func ftype fparams body) = do
    putWord32le 0
    putWord32le 0
    putWord8 40
    putWord8 09
    putWord8 02
    putWord32le 05
  get = error "get Type"

putOp :: (Binop, Type) -> Put
putOp (op, I32) = case op of
  Add -> putWord8 0x40

instance Serialize Expr where
  put x = case x of
    Nop                -> todo
    Unreachable        -> todo
    Block y1 y2        -> todo
    Break y1 y2        -> todo
    If y1 y2           -> todo
    IfElse y1 y2 y3    -> todo
    BrIf y1 y2 y3      -> todo
    Loop y1 y2 y3      -> todo
    Br y1 y2           -> todo
    Return y           -> todo
    Call y1 y2         -> todo
    Const y1 y2        -> todo
    Lit y              -> todo
    Load y1 y2         -> todo
    Store y1 y2        -> todo
    GetLocal y         -> todo
    SetLocal y1 y2     -> todo
    LoadExtend y1 y2   -> todo
    StoreWrap y1 y2 y3 -> todo
    Un y1 y2 y3        -> todo
    Rel y1 y2 y3 y4    -> todo
    Sel y1 y2 y3 y4    -> todo
    Convert y1 y2      -> todo
    Host y1 y2         -> todo
    Bin op ty x1 x2    -> do
      putOp (op, ty)
      put x1
      put x2
  get = error "get Type"

instance Serialize Module where
  put (Module funs imps exps) = do
    -- Decode Section
    put SectionSignatures
    putWord8 0x01             -- num signatures

    -- Signature[0]
    putWord8 0x00             -- num params
    put I32                   -- result type

    put SectionFunction
    putWord8 1                -- num functions

    -- Function[0]
    putWord16le 0x0009        -- function decl
    putWord32le 0x00001500
    putWord16le 0x0500

    putWord16le 0x4000 -- ExprI32Add
    putWord16le 0x0109 -- ExprI8Const(1)
    putWord16le 0x0209 -- ExprI8Const(2)
    put SectionEnd
    {-putWord8 0x6       -- WasmSectionEnd-}

    -- export name
    putWord8 0x74
    putWord8 0x65
    putWord8 0x73
    putWord8 0x74
  get = error "get Module"

todo :: t
todo = error "Not implemented"
