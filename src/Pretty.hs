{-# LANGUAGE FlexibleInstances #-}
-- Pretty printer for Textual AST.

module Pretty where
import qualified Data.Text                    as T
import           Prelude                      hiding ((<$>))
import           Syntax
import           Text.PrettyPrint.ANSI.Leijen



instance Pretty Name where
  pretty (Name t) = text (T.unpack t)

instance Pretty Expr where
  prettyList f = vsep (map (parens . pretty) f)
  pretty expr = case expr of
                  Nop -> text "nop"
                  Unreachable  -> text "unreachable"
                  Block _ _ -> text "todo"
                  Break name _ -> text "todo"
                  If cond true -> text "todo"
                  IfElse cond true false -> text "todo"
                  BrIf cond name e -> text "todo"
                  Loop{} -> text "todo"
                  Br name _ -> text "todo"
                  Return e -> text "return" <+> pretty e
                  Call name _ -> text "todo"
                  Const typ value -> pretty value
                  Lit value -> pretty value
                  Load memop e -> text "todo"
                  Store memop e -> text "todo"
                  GetLocal name -> text "todo"
                  SetLocal name e -> text "todo"
                  LoadExtend extop e -> text "todo"
                  StoreWrap wrapop e1 e2 -> text "todo"
                  Bin binop typ e1 e2 -> pretty typ <> dot <> pretty binop <+> pretty e1 <+> pretty e2
                  Un unop typ e -> pretty typ <> dot <> pretty unop <+> pretty e
                  Rel relop typ e e1 -> text "todo"
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
                  LtS -> text "ltS"
                  LtU -> text "ltU"
                  LeS -> text "leS"
                  LeU -> text "leU"
                  GtS -> text "gtS"
                  GtU -> text "gtU"
                  GeS -> text "geS"
                  GeU -> text "geU"
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
  prettyList f = vsep (map (parens . pretty) f)
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
  pretty param = case param of
                  (Param (Just name) typ) -> text  "param" <+> pretty name <+> pretty typ
                  (Param Nothing typ) -> text  "param" <+> pretty typ
                  (Result typ) -> text  "result" <+> pretty typ
                  (Body expr) -> pretty expr
  prettyList f = fillCat (map (parens . pretty) f)

instance Pretty Type where
  pretty ty = case ty of
                Void -> text "void"
                I32  -> text "i32"
                I64  -> text "i64"
                F32  -> text "f32"
                F64  -> text "f64"
                All  -> text "all"
