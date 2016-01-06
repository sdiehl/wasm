module Binary where

import Data.Word
{-import Data.Binary-}
import Data.ByteString
import Data.Serialize
import qualified Data.List as List

import Syntax

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
  put t = case t of
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
  get = error "get Type"

instance Serialize Func where
  put (Export _ _) = do
    putWord8 00
    putWord8 01
    putWord8 02
    putWord8 01
    putWord8 09
  put (Func ftype fparams body) = do
    putWord32be 0
    putWord64be 0
    putWord32be 0
    putWord8 40
    putWord8 09
    putWord8 02
    putWord32le 05
  get = error "get Type"

instance Serialize Module where
  put (Module funs imps exps) = do
    put SectionSignatures
    putWord8 1
    mapM_ put funs
    {-putWord8 $ fromIntegral (List.length funs)-}
  get = error "get Module"
