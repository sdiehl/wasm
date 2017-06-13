{-# LANGUAGE FlexibleInstances #-}
-- Pretty printer for Textual AST.

module Language.Wasm.Pretty where
import qualified Data.Text as T
import Language.Wasm.Syntax
import Prelude hiding ((<$>))
import Text.PrettyPrint.ANSI.Leijen



instance Pretty Name where
  pretty (Name t) = text (T.unpack t)

instance Pretty Expr where
  prettyList = nest 4 . vsep . map (parens . pretty)
  pretty expr = case expr of
                  Nop -> text "nop"
                  Unreachable  -> text "unreachable"
                  Block name e -> text "block" <+> maybe empty pretty name <$> pretty e
                  If cond true -> text "if" <+> pretty [cond,true]
                  IfElse cond true false -> text "if_else" <+> pretty [cond,true,false]
                  BrIf cond name e -> text "todo"
                  Loop{} -> text "todo"
                  Br name e -> pretty [text "br" <+> pretty name]
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
                  Convert cvtop ty e -> text "todo"
                  Host hostop _ -> text "todo"


instance Pretty BinOp where
  pretty binop = text $ case binop of
                  Add      -> "add"
                  Sub      -> "sub"
                  Mul      -> "mul"
                  DivS     -> "divs"
                  DivU     -> "divu"
                  RemS     -> "rems"
                  RemU     -> "remu"
                  And      -> "and"
                  Or       -> "or"
                  Xor      -> "xor"
                  Shl      -> "shl"
                  ShrU     -> "shr_u"
                  ShrS     -> "shr_s"
                  RotL     -> "rotl"
                  RotR     -> "Rotr"
                  Div      -> "div"
                  CopySign -> "copysign"
                  Min      -> "min"
                  Max      -> "max"

instance Pretty UnOp where
  pretty unop = text $ case unop of
                  Clz     -> "clz"
                  Ctz     -> "ctz"
                  Popcnt  -> "popcnt"
                  Neg     -> "neg"
                  Abs     -> "abs"
                  Ceil    -> "ceil"
                  Floor   -> "floor"
                  Trunc   -> "trunc"
                  Nearest -> "nearest"
                  Sqrt    -> "sqrt"

instance Pretty RelOp where
  pretty relop = text $ case relop of
                  Eqz  -> "eqz"
                  Eq  -> "eq"
                  Ne  -> "ne"
                  LtS -> "lts"
                  LtU -> "ltu"
                  LeS -> "les"
                  LeU -> "leu"
                  GtS -> "gts"
                  GtU -> "gtu"
                  GeS -> "ges"
                  GeU -> "geu"
                  Lt  -> "lt"
                  Le  -> "le"
                  Gt  -> "gt"
                  Ge  -> "ge"

instance Pretty Value where
  pretty value = text $ case value of
                  VI32 v -> show v
                  VI64 v -> show v
                  VF32 v -> show v
                  VF64 v -> show v

instance Pretty Func where
  prettyList = vsep . map (parens . pretty)
  pretty f = case f of
              Export name value -> text "export" <+> pretty name <+> pretty value
              Import name value -> text "Import" <+> pretty name <+> pretty value
              Func name params body -> nest 4 (text "func" <+> maybe empty pretty name <+> pretty params <$> pretty body )

instance Pretty Decl where
  pretty d = case d of
              ModDecl m     -> pretty m
              ExprDecl expr -> pretty expr

instance Pretty Module where
  pretty Module { _funcs=funcs } = parens (nest 4 (text "module" <$> pretty funcs))

instance Pretty Param where
  prettyList = fillSep . map (parens . pretty)
  pretty param = case param of
                  Param (Just name) typ -> text  "param" <+> pretty name <+> pretty typ
                  Param Nothing typ -> text  "param" <+> pretty typ
                  Result typ -> text  "result" <+> pretty typ
                  Body expr -> pretty expr

instance Pretty Type where
  pretty ty = text $ case ty of
                Void -> "void"
                I32  -> "i32"
                I64  -> "i64"
                F32  -> "f32"
                F64  -> "f64"
                All  -> "all"
