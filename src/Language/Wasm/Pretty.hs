{-# LANGUAGE FlexibleInstances #-}
-- Pretty printer for Textual AST.

module Language.Wasm.Pretty where
import Data.Maybe
import qualified Data.Text as T
import Language.Wasm.Syntax
import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen


instance Pretty Name where
  prettyList = fillSep . map pretty
  pretty (Name t) = text (T.unpack t)

instance Pretty Expr where
  prettyList = nest 2 . vsep . map (parens . pretty)
  pretty expr = case expr of
                  Nop -> text "nop"
                  Unreachable  -> text "unreachable"
                  Block name e -> text "block" <+> pretty name <$> pretty e
                  If cond true -> text "if" <+> pretty [cond,true]
                  IfElse cond true false -> text "if_else" <+> pretty [cond,true,false]
                  BrIf cond name e -> text "todo"
                  Loop name1 name2 e-> text "loop" <+> pretty (catMaybes [name1, name2]) <$> pretty e
                  Br name e -> text "br" <+> pretty name <+> pretty e
                  Return e -> text "return" <+> parens (pretty e)
                  Call name _ -> text "todo"
                  Const typ value -> pretty typ <> dot <> text "const" <+> pretty value
                  Lit value -> pretty value
                  Load memop e -> text "todo"
                  Store memop e -> text "todo"
                  GetLocal name -> text "get_local" <+> pretty name
                  SetLocal name e -> text "set_local" <+> pretty name <+> parens (pretty e)
                  LoadExtend extop e -> text "todo"
                  StoreWrap wrapop e1 e2 -> text "todo"
                  Bin binop typ e1 e2 -> pretty typ <> dot <> pretty binop <+> parens (pretty e1) <+> parens( pretty e2)
                  Un unop typ e -> pretty typ <> dot <> pretty unop <+> parens (pretty e)
                  Rel relop typ e1 e2 -> pretty typ <> dot <> pretty relop <+> parens (pretty e1) <+> parens( pretty e2)
                  Sel selop e1 e2 e3 -> text "todo"
                  Convert cvtop e -> text "todo"
                  Host hostop _ -> text "todo"


instance Pretty Binop where
  pretty binop = case binop of
                  Add      -> text "add"
                  Sub      -> text "sub"
                  Mul      -> text "mul"
                  DivS     -> text "divs"
                  DivU     -> text "divu"
                  RemS     -> text "rems"
                  RemU     -> text "remu"
                  And      -> text "and"
                  Or       -> text "or"
                  Xor      -> text "xor"
                  Shl      -> text "shl"
                  ShrU     -> text "shru"
                  ShrS     -> text "shrs"
                  Div      -> text "div"
                  CopySign -> text "copysign"
                  Min      -> text "min"
                  Max      -> text "max"

instance Pretty Unop where
  pretty unop = case unop of
                  Clz     -> text "clz"
                  Ctz     -> text "ctz"
                  Popcnt  -> text "popcnt"
                  Neg     -> text "neg"
                  Abs     -> text "abs"
                  Ceil    -> text "ceil"
                  Floor   -> text "floor"
                  Trunc   -> text "trunc"
                  Nearest -> text "nearest"
                  Sqrt    -> text "sqrt"

instance Pretty Relop where
  pretty relop = case relop of
                  Eq  -> text "eq"
                  Ne  -> text "ne"
                  LtS -> text "lts"
                  LtU -> text "ltu"
                  LeS -> text "les"
                  LeU -> text "leu"
                  GtS -> text "gts"
                  GtU -> text "gtu"
                  GeS -> text "ges"
                  GeU -> text "geu"
                  Lt  -> text "lt"
                  Le  -> text "le"
                  Gt  -> text "gt"
                  Ge  -> text "ge"

instance Pretty Value where
  pretty value = case value of
                  (VI32 v) -> text (show v)
                  (VI64 v) -> text (show v)
                  (VF32 v) -> text (show v)
                  (VF64 v) -> text (show v)

instance Pretty Func where
  prettyList = vsep . map (parens . pretty)
  pretty f = case f of
              (Export name value) -> text "export" <+> pretty name <+> pretty value
              (Import name value) -> text "Import" <+> pretty name <+> pretty value
              (Func name params body) -> nest 2 (text "func" <+> maybe empty pretty name <+> pretty params <$> pretty body )

instance Pretty Decl where
  pretty d = case d of
              (ModDecl m)     -> pretty m
              (ExprDecl expr) -> pretty expr

instance Pretty Module where
  pretty Module { _funcs=funcs } = parens (nest 2 (text "module" <$> pretty funcs))

instance Pretty Param where
  prettyList = fillSep . map (parens . pretty)
  pretty param = case param of
                  (Param (Just name) typ) -> text  "param" <+> pretty name <+> pretty typ
                  (Param Nothing typ) -> text  "param" <+> pretty typ
                  (Result typ) -> text  "result" <+> pretty typ
                  (Body expr) -> pretty expr

instance Pretty Type where
  pretty ty = case ty of
                Void -> text "void"
                I32  -> text "i32"
                I64  -> text "i64"
                F32  -> text "f32"
                F64  -> text "f64"
                All  -> text "all"
