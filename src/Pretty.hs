module Pretty (prettyPrint) where

import AST
-- Ocultamos o <> do Prelude para não conflitar com o do PrettyPrint
import Prelude hiding ((<>))
import Text.PrettyPrint

-- Função Principal exportada
prettyPrint :: Program -> String
prettyPrint prog = render (vcat (map ppTopDecl prog))

-- =============================================================================
-- TOP LEVEL (Funções e Structs)
-- =============================================================================

ppTopDecl :: TopDecl -> Doc
ppTopDecl (FuncDecl name generics params retType body) =
    let header = ppGenerics generics <+> text "func" <+> text name <> parens (ppParams params) <> colon <+> ppType retType
    in ppBlockWithHeader header body

ppTopDecl (StructDecl name fields) =
    let header = text "struct" <+> text name
    in header <+> lbrace $$ nest 4 (vcat (map ppStructField fields)) $$ rbrace

ppGenerics :: [String] -> Doc
ppGenerics [] = empty
ppGenerics vars = text "forall" <+> hsep (map text vars)

ppParams :: [(String, Type)] -> Doc
ppParams params = hsep (punctuate comma (map ppParam params))

ppParam :: (String, Type) -> Doc
ppParam (name, typ) = text name <+> ppType typ

ppStructField :: (String, Type) -> Doc
ppStructField (name, typ) = (text name <+> ppType typ) <> semi

-- =============================================================================
-- TYPES
-- =============================================================================

ppType :: Type -> Doc
ppType TyInt = text "int"
ppType TyFloat = text "float"
ppType TyString = text "string"
ppType TyBool = text "bool"
ppType TyVoid = text "void"
ppType (TyCustom s) = text s
ppType (TyArray t) = ppType t <> text "[]"
ppType (TyVar s) = text s
ppType (TyFunc args ret) = 
    parens (hcat (punctuate comma (map ppType args))) <+> text "->" <+> ppType ret

-- =============================================================================
-- STATEMENTS & BLOCKS
-- =============================================================================

-- Função auxiliar para imprimir blocos { ... } com indentação correta
-- Formato:
-- header {
--     body
-- }
ppBlockWithHeader :: Doc -> Block -> Doc
ppBlockWithHeader header stmts =
    (header <+> lbrace)
    $$ nest 4 (ppBlockBody stmts)
    $$ rbrace

ppBlockBody :: [Stmt] -> Doc
ppBlockBody stmts = vcat (map ppStmt stmts)

ppStmt :: Stmt -> Doc
ppStmt (VarDecl name typ initExpr) =
    (text "let" <+> text name <+> ppType typ <+> ppInit initExpr) <> semi
  where
    ppInit Nothing = empty
    ppInit (Just e) = equals <+> ppExpr e

ppStmt (Assign lhs rhs) =
    (ppExpr lhs <+> equals <+> ppExpr rhs) <> semi

ppStmt (If cond thenBlock Nothing) =
    let header = text "if" <+> parens (ppExpr cond)
    in ppBlockWithHeader header thenBlock

ppStmt (If cond thenBlock (Just elseBlock)) =
    let ifPart = ppBlockWithHeader (text "if" <+> parens (ppExpr cond)) thenBlock
        elsePart = ppBlockWithHeader (text "else") elseBlock
    in ifPart $$ elsePart

ppStmt (While cond body) =
    let header = text "while" <+> parens (ppExpr cond)
    in ppBlockWithHeader header body

ppStmt (For initStmt cond step body) =
    let header = text "for" <+> parens ((ppOptStmt initStmt <> semi) <+> (ppOptExpr cond <> semi) <+> ppOptStmt step)
    in ppBlockWithHeader header body

ppStmt (Return Nothing) = text "return" <> semi
ppStmt (Return (Just e)) = (text "return" <+> ppExpr e) <> semi

ppStmt (ExprStmt e) = ppExpr e <> semi

-- Helpers
ppOptStmt :: Maybe Stmt -> Doc
ppOptStmt Nothing = empty
ppOptStmt (Just s) = 
    let doc = ppStmt s
        sDoc = render doc
    in text (if not (null sDoc) && last sDoc == ';' then init sDoc else sDoc)

ppOptExpr :: Maybe Expr -> Doc
ppOptExpr Nothing = empty
ppOptExpr (Just e) = ppExpr e

-- =============================================================================
-- EXPRESSIONS
-- =============================================================================

ppExpr :: Expr -> Doc
ppExpr (LitInt i) = integer i
ppExpr (LitFloat f) = double f
ppExpr (LitBool True) = text "true"
ppExpr (LitBool False) = text "false"
ppExpr (LitString s) = doubleQuotes (text s)
ppExpr (Var s) = text s
ppExpr (ArrayLit exprs) = brackets (hcat (punctuate comma (map ppExpr exprs)))

ppExpr (Binary op e1 e2) = 
    ppExpr e1 <+> ppBinOp op <+> ppExpr e2

ppExpr (Unary op e) = 
    ppUnOp op <> ppExpr e

ppExpr (ArrayAccess arr idx) =
    ppExpr arr <> brackets (ppExpr idx)

ppExpr (FieldAccess obj field) =
    ppExpr obj <> dot <> text field

ppExpr (Call name args) =
    text name <> parens (hcat (punctuate comma (map ppExpr args)))

ppExpr (New typ size) =
    (text "new" <+> ppType typ) <> brackets (ppExpr size)

-- Operadores
ppBinOp :: BinOp -> Doc
ppBinOp Add = char '+'
ppBinOp Sub = char '-'
ppBinOp Mul = char '*'
ppBinOp Div = char '/'
ppBinOp Mod = char '%'
ppBinOp Eq  = text "=="
ppBinOp Neq = text "!="
ppBinOp Lt  = char '<'
ppBinOp Gt  = char '>'
ppBinOp Le  = text "<="
ppBinOp Ge  = text ">="
ppBinOp And = text "&&"
ppBinOp Or  = text "||"

ppUnOp :: UnOp -> Doc
ppUnOp Neg = char '-'
ppUnOp Not = char '!'

-- Helper manual para 'dot'
dot :: Doc
dot = char '.'