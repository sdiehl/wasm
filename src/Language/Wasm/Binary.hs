{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Wasm.Binary where

import Data.ByteString
import qualified Data.List as List
import Data.Serialize
import Data.Word

import Language.Wasm.Core


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
  Eqz -> putWord8 0x45
  Eq  -> putWord8 0x46
  Ne  -> putWord8 0x47
  LtS -> putWord8 0x48
  LtU-> putWord8 0x49
  GtS -> putWord8 0x4a
  GtU-> putWord8 0x4b
  LeS -> putWord8 0x4c
  LeU-> putWord8 0x4d
  GeS -> putWord8 0x4e
  GeU-> putWord8 0x4f
  _   -> todo -- actually not supported
relOp (op, I64) = case op of
  Eqz -> putWord8 0x50
  Eq  -> putWord8 0x51
  Ne  -> putWord8 0x52
  LtS -> putWord8 0x53
  LtU-> putWord8 0x54
  GtS -> putWord8 0x55
  GtU-> putWord8 0x56
  LeS -> putWord8 0x57
  LeU-> putWord8 0x58
  GeS -> putWord8 0x59
  GeU-> putWord8 0x5a
  _   -> todo -- actually not supported
relOp (op, F32) = case op of
  Eq -> putWord8 0x5b
  Ne -> putWord8 0x5c
  Lt -> putWord8 0x5d
  Gt -> putWord8 0x5e
  Le -> putWord8 0x5f
  Ge -> putWord8 0x60
  _  -> todo -- actually not supported
relOp (op, F64) = case op of
  Eq -> putWord8 0x61
  Ne -> putWord8 0x62
  Lt -> putWord8 0x63
  Gt -> putWord8 0x64
  Le -> putWord8 0x65
  Ge -> putWord8 0x66
  _  -> todo -- actually not supported


unOp :: (Unop, Type) -> Put
unOp (op, I32) = case op of
  Clz    -> putWord8 0x67
  Ctz    -> putWord8 0x68
  Popcnt -> putWord8 0x69
unOp (op, I64) = case op of
  Clz    -> putWord8 0x79
  Ctz    -> putWord8 0x7a
  Popcnt -> putWord8 0x7b
unOp (op, F32) = case op of
  Abs     -> putWord8 0x8b
  Neg     -> putWord8 0x8c
  Ceil    -> putWord8 0x8d
  Floor   -> putWord8 0x8e
  Trunc   -> putWord8 0x8f
  Nearest -> putWord8 0x90
  Sqrt    -> putWord8 0x91
unOp (op, F64) = case op of
  Abs     -> putWord8 0x99
  Neg     -> putWord8 0x9a
  Ceil    -> putWord8 0x9b
  Floor   -> putWord8 0x9c
  Trunc   -> putWord8 0x9d
  Nearest -> putWord8 0x9e
  Sqrt    -> putWord8 0x9f

binOp :: (Binop, Type) -> Put
binOp (op, I32) = case op of
  Add  -> putWord8 0x6a
  Sub  -> putWord8 0x6b
  Mul  -> putWord8 0x6c
  DivS -> putWord8 0x6d
  DivU -> putWord8 0x6e
  RemS -> putWord8 0x6f
  RemU -> putWord8 0x70
  And  -> putWord8 0x71
  Or   -> putWord8 0x72
  Xor  -> putWord8 0x73
  Shl  -> putWord8 0x74
  ShrS -> putWord8 0x75
  ShrU -> putWord8 0x76
  RotL -> putWord8 0x77
  RotR -> putWord8 0x78
  _    -> todo -- actually not supported
binOp (op, I64) = case op of
  Add  -> putWord8 0x7c
  Sub  -> putWord8 0x7d
  Mul  -> putWord8 0x7e
  DivS -> putWord8 0x7f
  DivU -> putWord8 0x80
  RemS -> putWord8 0x81
  RemU -> putWord8 0x82
  And  -> putWord8 0x83
  Or   -> putWord8 0x84
  Xor  -> putWord8 0x85
  Shl  -> putWord8 0x86
  ShrS -> putWord8 0x87
  ShrU -> putWord8 0x88
  RotL -> putWord8 0x89
  RotR -> putWord8 0x8a
  _    -> todo -- actually not supported
binOp (op, F32) = case op of
  Add      -> putWord8 0x92
  Sub      -> putWord8 0x93
  Mul      -> putWord8 0x94
  Div      -> putWord8 0x95
  Min      -> putWord8 0x96
  Max      -> putWord8 0x97
  CopySign -> putWord8 0x98
  _        -> todo -- actually not supported
binOp (op, F64) = case op of
  Add      -> putWord8 0xa0
  Sub      -> putWord8 0xa1
  Mul      -> putWord8 0xa2
  Div      -> putWord8 0xa3
  Min      -> putWord8 0xa4
  Max      -> putWord8 0xa5
  CopySign -> putWord8 0xa6
  _        -> todo -- actually not supported

instance Serialize Value where
  put x = case x of
    VI32 y -> putWord8 (fromIntegral y)
    VI64 y -> putWord8 (fromIntegral y)
    VF32 y -> todo
    VF64 y -> todo
  get = error "get Type"

instance Serialize Expr where
  put x = case x of
    Nop                -> todo
    Unreachable        -> todo
    Block y1 y2        -> todo
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
    Un op ty x1  -> do
      unOp (op, ty)
      put x1


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
