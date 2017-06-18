{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Wasm.Binary where

import Data.ByteString
import Data.Serialize
import Data.Word

import Language.Wasm.Core

{-

See:

https://webassembly.github.io/spec/binary/instructions.html

-}

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
  put x = putWord8 $ case x of
    FunctionFlagName   -> 1
    FunctionFlagImport -> 2
    FunctionFlagLocals -> 4
    FunctionFlagExport -> 8
  get = error "get Type"

instance Serialize WasmSectionType where
  put x = putWord8 $ case x of
    SectionMemory        -> 0
    SectionSignatures    -> 1
    SectionFunction      -> 2
    SectionGlobals       -> 3
    SectionDataSegments  -> 4
    SectionFunctionTable -> 5
    SectionEnd           -> 6
  get = error "get Type"

instance Serialize Type where
  put t = putWord8 $ case t of
    Void -> 0
    I32  -> 0x7f
    I64  -> 0x7e
    F32  -> 0x7d
    F64  -> 0x7c
    All  -> 0x70
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


relOp :: (RelOp, Type) -> Put
relOp (op, I32) = putWord8 $ case op of
  Eqz -> 0x45
  Eq  -> 0x46
  Ne  -> 0x47
  LtS -> 0x48
  LtU -> 0x49
  GtS -> 0x4a
  GtU -> 0x4b
  LeS -> 0x4c
  LeU -> 0x4d
  GeS -> 0x4e
  GeU -> 0x4f
  _   -> todo -- not supported
relOp (op, I64) = putWord8 $ case op of
  Eqz -> 0x50
  Eq  -> 0x51
  Ne  -> 0x52
  LtS -> 0x53
  LtU -> 0x54
  GtS -> 0x55
  GtU -> 0x56
  LeS -> 0x57
  LeU -> 0x58
  GeS -> 0x59
  GeU -> 0x5a
  _   -> todo -- not supported
relOp (op, F32) = putWord8 $ case op of
  Eq -> 0x5b
  Ne -> 0x5c
  Lt -> 0x5d
  Gt -> 0x5e
  Le -> 0x5f
  Ge -> 0x60
  _  -> todo -- not supported
relOp (op, F64) = putWord8 $ case op of
  Eq -> 0x61
  Ne -> 0x62
  Lt -> 0x63
  Gt -> 0x64
  Le -> 0x65
  Ge -> 0x66
  _  -> todo -- not supported
relOp (_, _) = todo


unOp :: (UnOp, Type) -> Put
unOp (op, I32) = putWord8 $ case op of
  Clz    -> 0x67
  Ctz    -> 0x68
  Popcnt -> 0x69
  _      -> todo -- not supported
unOp (op, I64) = putWord8 $ case op of
  Clz    -> 0x79
  Ctz    -> 0x7a
  Popcnt -> 0x7b
  _      -> todo -- not supported
unOp (op, F32) = putWord8 $ case op of
  Abs     -> 0x8b
  Neg     -> 0x8c
  Ceil    -> 0x8d
  Floor   -> 0x8e
  Trunc   -> 0x8f
  Nearest -> 0x90
  Sqrt    -> 0x91
  _       -> todo -- not supported
unOp (op, F64) = putWord8 $ case op of
  Abs     -> 0x99
  Neg     -> 0x9a
  Ceil    -> 0x9b
  Floor   -> 0x9c
  Trunc   -> 0x9d
  Nearest -> 0x9e
  Sqrt    -> 0x9f
  _       -> todo -- not supported
unOp (_, _) = todo


binOp :: (BinOp, Type) -> Put
binOp (op, I32) = putWord8 $ case op of
  Add  -> 0x6a
  Sub  -> 0x6b
  Mul  -> 0x6c
  DivS -> 0x6d
  DivU -> 0x6e
  RemS -> 0x6f
  RemU -> 0x70
  And  -> 0x71
  Or   -> 0x72
  Xor  -> 0x73
  Shl  -> 0x74
  ShrS -> 0x75
  ShrU -> 0x76
  RotL -> 0x77
  RotR -> 0x78
  _    -> todo -- not supported
binOp (op, I64) = putWord8 $ case op of
  Add  -> 0x7c
  Sub  -> 0x7d
  Mul  -> 0x7e
  DivS -> 0x7f
  DivU -> 0x80
  RemS -> 0x81
  RemU -> 0x82
  And  -> 0x83
  Or   -> 0x84
  Xor  -> 0x85
  Shl  -> 0x86
  ShrS -> 0x87
  ShrU -> 0x88
  RotL -> 0x89
  RotR -> 0x8a
  _    -> todo -- not supported
binOp (op, F32) = putWord8 $ case op of
  Add      -> 0x92
  Sub      -> 0x93
  Mul      -> 0x94
  Div      -> 0x95
  Min      -> 0x96
  Max      -> 0x97
  CopySign -> 0x98
  _        -> todo -- not supported
binOp (op, F64) = putWord8 $ case op of
  Add      -> 0xa0
  Sub      -> 0xa1
  Mul      -> 0xa2
  Div      -> 0xa3
  Min      -> 0xa4
  Max      -> 0xa5
  CopySign -> 0xa6
  _        -> todo -- not supported
binOp (_, _) = todo


convertOp :: (ConvertOp, Type) -> Put
convertOp (op, I32) = putWord8 $ case op of
  WrapI64        -> 0xa7
  TruncSF32      -> 0xa8
  TruncUF32      -> 0xa9
  TruncSF64      -> 0xaa
  TruncUF64      -> 0xab
  ReinterpretF32 -> 0xbc
  _              -> todo -- not supported
convertOp (op, I64) = putWord8 $ case op of
  ExtendSI32     -> 0xac
  ExtendUI32     -> 0xad
  TruncSF32      -> 0xae
  TruncUF32      -> 0xaf
  TruncSF64      -> 0xb0
  TruncUF64      -> 0xb1
  ReinterpretF64 -> 0xbd
  _              -> todo -- not supported
convertOp (op, F32) = putWord8 $ case op of
  ConvertSI32    -> 0xb2
  ConvertUI32    -> 0xb3
  ConvertSI64    -> 0xb4
  ConvertUI64    -> 0xb5
  DemoteF64      -> 0xb6
  ReinterpretI32 -> 0xbe
  _              -> todo -- not supported
convertOp (op, F64) = putWord8 $ case op of
  ConvertSI32    -> 0xb7
  ConvertUI32    -> 0xb8
  ConvertSI64    -> 0xb9
  ConvertUI64    -> 0xba
  PromoteF32     -> 0xbb
  ReinterpretI64 -> 0xbf
  _              -> todo -- not supported
convertOp (_, _) = todo

instance Serialize Value where
  put x = putWord8 $ case x of
    VI32 y -> (fromIntegral y)
    VI64 y -> (fromIntegral y)
    VF32 y -> todo
    VF64 y -> todo
  get = error "get Type"

instance Serialize Expr where
  put x = case x of
    Unreachable        -> putWord8 0x00
    Nop                -> putWord8 0x01

    Block y1 y2        -> do
      put y2
      putWord8 0x0b

    If y1 y2           -> do
      put y2
      putWord8 0x0b

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
          putWord8 0x41
          put val

        I64 -> do
          putWord8 0x42
          put val

        F32 -> do
          putWord8 0x43
          put val

        F64 -> do
          putWord8 0x44
          put val

    Lit y              -> todo
    Load y1 y2         -> todo
    Store y1 y2        -> todo

    GetLocal y         -> do
      putWord8 0x20
      put y

    SetLocal y1 y2     -> do
      putWord8 0x21
      put y1

    LoadExtend y1 y2   -> todo
    StoreWrap y1 y2 y3 -> todo
    Un op ty x  -> do
      unOp (op, ty)
      put x

    Rel op ty x1 x2    -> do
      relOp (op, ty)
      put x1
      put x2

    Sel y1 y2 y3 y4    -> todo
    Convert op ty x      -> todo
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

instance Serialize Name where
  put (UnName x) = putWord32le x
  put (Name x) = todo
  get = UnName <$> getWord32le

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
