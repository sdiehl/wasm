{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}

module Binary where

import Data.Word
import Data.ByteString
import Data.Serialize
import Data.Serialize.Put
import qualified Data.List as List

import Core
{-import Syntax-}

-------------------------------------------------------------------------------
-- Binary Writer
-------------------------------------------------------------------------------

magic :: ByteString
magic = "\0asm"

magicHex :: Integer
magicHex = 0x6d736100

magic0, magic1, magic2, magic3 :: Word8
magic0 = 0x00  -- '\0'
magic1 = 0x61  -- 'a'
magic2 = 0x73  -- 's'
magic3 = 0x6d  -- 'm'

data WasmSectionType
  = SectionMemory
  | SectionSignatures
  | SectionFunction
  | SectionGlobals
  | SectionDataSegments
  | SectionFunctionTable
  | SectionEnd
  deriving (Eq, Show)

data WasmFunctionFlag
  = FunctionFlagName
  | FunctionFlagImport
  | FunctionFlagLocals
  | FunctionFlagExport
  deriving (Eq, Show)

instance Serialize WasmFunctionFlag where
  put x = case x of
    FunctionFlagName   -> putWord8 1
    FunctionFlagImport -> putWord8 2
    FunctionFlagLocals -> putWord8 4
    FunctionFlagExport -> putWord8 8
  get = error "get Type"

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
  put (ModDecl mod) = put mod
  put (ExprDecl mod) = do
    undefined
  get = error "get Type"

instance Serialize Func where
  {-put (Export name _) = do-}
    {-putWord8 0x74-}
    {-putWord8 0x65-}
    {-putWord8 0x73-}
    {-putWord8 0x74-}

  put (Func fname fparams fbody) = do
    let (_, bs) = runPutM $ mapM_ put fbody
    let len = Data.ByteString.length bs

    putWord8 09
    putWord16le 0x0000          -- function signature index
    putWord32le 0x00000015      -- function name offset
    putWord8 (fromIntegral len) -- function body size
    putWord8 0x00

    putByteString bs

  get = error "get Type"

{-instance Serialize Param where-}
  {-put (Body x) = put x-}
  {-put (Result x) = return ()-}
  {-get = error "get Type"-}

relOp :: (Relop, Type) -> Put
relOp (op, I32) = case op of
  Eq  -> putWord8 0x4d
  Ne  -> todo
  LtS -> todo
  LtU -> todo
  LeS -> todo
  LeU -> todo
  GtS -> todo
  GtU -> todo
  GeS -> todo
  GeU -> todo
  Lt  -> todo
  Le  -> todo
  Gt  -> todo
  Ge  -> todo
relOp (op, I64) = case op of
  Eq  -> putWord8 0x68
  Ne  -> todo
  LtS -> todo
  LtU -> todo
  LeS -> todo
  LeU -> todo
  GtS -> todo
  GtU -> todo
  GeS -> todo
  GeU -> todo
  Lt  -> todo
  Le  -> todo
  Gt  -> todo
  Ge  -> todo

binOp :: (Binop, Type) -> Put
binOp (op, I32) = case op of
  Add      -> putWord8 0x40
  Sub      -> todo
  Mul      -> putWord8 0x42
  DivS     -> todo
  DivU     -> todo
  RemS     -> todo
  RemU     -> todo
  And      -> todo
  Or       -> todo
  Xor      -> todo
  Shl      -> todo
  ShrU     -> todo
  ShrS     -> todo
  Div      -> todo
  CopySign -> todo
  Min      -> todo
  Max      -> todo
binOp (op, I64) = case op of
  Add      -> putWord8 0x5b
  Sub      -> putWord8 0x5c
  Mul      -> putWord8 0x5d
  DivS     -> todo
  DivU     -> todo
  RemS     -> todo
  RemU     -> todo
  And      -> todo
  Or       -> todo
  Xor      -> todo
  Shl      -> todo
  ShrU     -> todo
  ShrS     -> todo
  Div      -> todo
  CopySign -> todo
  Min      -> todo
  Max      -> todo

instance Serialize Value where
  put x = case x of
    VI32 y   -> putWord8 (fromIntegral y)
    VI64 y   -> putWord8 (fromIntegral y)
    VF32 y   -> todo
    VF64 y   -> todo
  get = error "get Type"

instance Serialize Expr where
  put x = case x of
    Nop                -> todo
    Unreachable        -> todo
    Block y1 y2        -> todo
    Break y1 y2        -> todo
    If y1 y2           -> todo

    IfElse cond tr fl  -> do
      putWord8 0x4
      put cond
      put tr
      put fl

    BrIf y1 y2 y3      -> todo
    Loop y1 y2 y3      -> todo
    Br y1 y2           -> todo
    Return y           -> todo

    Call fn args       -> do
      putWord8 0x12
      mapM_ put args

    Const ty val       -> do
      case ty of
        I32 -> do
          putWord8 0x09
          put val

        I64 -> do
          putWord8 0x0b
          put val

    Lit y              -> todo
    Load y1 y2         -> todo
    Store y1 y2        -> todo

    GetLocal y         -> do
      putWord8 0x0e

    SetLocal y1 y2     -> todo
    LoadExtend y1 y2   -> todo
    StoreWrap y1 y2 y3 -> todo
    Un y1 y2 y3        -> todo

    Rel op ty x1 x2    -> do
      relOp (op, ty)
      put x1
      put x2

    Sel y1 y2 y3 y4    -> todo
    Convert y1 y2      -> todo
    Host y1 y2         -> todo

    Bin op ty x1 x2 -> do
      binOp (op, ty)
      put x1
      put x2

  get = error "get Type"

instance Serialize Export where
  put _ = do
    putWord8 0x74
    putWord8 0x65
    putWord8 0x73
    putWord8 0x74
  get = error "get Type"

instance Serialize Import where
  put = error "get Type"
  get = error "get Type"

instance Serialize Module where
  put (Module funs imports exports) = do
    -- Magic
    putWord8 magic0
    putWord8 magic1
    putWord8 magic2
    putWord8 magic3

    -- Decode Section
    put SectionSignatures
    putWord8 0x01             -- num signatures

    -- Signature[0]
    putWord8 0x00             -- num params
    put I32                   -- result type

    put SectionFunction
    putWord8 1                -- num functions

    mapM_ put funs
    put SectionEnd
    mapM_ put exports
    putWord8 0x00

  get = error "get Module"

todo :: t
todo = error "Not implemented"
