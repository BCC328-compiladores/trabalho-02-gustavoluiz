module TreeUtils (astToTree) where

import AST
import Data.Tree 

-- Função principal que converte o Programa inteiro em uma Tree String
astToTree :: Program -> Tree String
astToTree decls = Node "Program" (map topDeclToTree decls)

-- =============================================================================
-- CONVERSORES PARA TOP LEVEL
-- =============================================================================

topDeclToTree :: TopDecl -> Tree String
topDeclToTree (FuncDecl name generics params retType body) =
    Node ("FuncDecl: " ++ name) [
        Node ("Generics: " ++ show generics) [],
        Node "Params" (map paramToTree params),
        Node ("ReturnType: " ++ show retType) [],
        Node "Body" (map stmtToTree body)
    ]

topDeclToTree (StructDecl name fields) =
    Node ("StructDecl: " ++ name) [
        Node "Fields" (map fieldToTree fields)
    ]

paramToTree :: (String, Type) -> Tree String
paramToTree (name, typ) = Node (name ++ " : " ++ show typ) []

fieldToTree :: (String, Type) -> Tree String
fieldToTree (name, typ) = Node (name ++ " : " ++ show typ) []

-- =============================================================================
-- CONVERSORES PARA STATEMENTS
-- =============================================================================

stmtToTree :: Stmt -> Tree String
stmtToTree (VarDecl name typ maybeInit) =
    Node ("VarDecl: " ++ name ++ " (" ++ show typ ++ ")") (initToTree maybeInit)
  where
    initToTree Nothing = []
    initToTree (Just expr) = [exprToTree expr]

stmtToTree (Assign lhs rhs) =
    Node "Assign" [exprToTree lhs, exprToTree rhs]

stmtToTree (If cond thenBlock maybeElse) =
    Node "If" ([
        Node "Condition" [exprToTree cond],
        Node "Then" (map stmtToTree thenBlock)
    ] ++ elseBlockToTree maybeElse)
  where
    elseBlockToTree Nothing = []
    elseBlockToTree (Just block) = [Node "Else" (map stmtToTree block)]

stmtToTree (While cond body) =
    Node "While" [
        Node "Condition" [exprToTree cond],
        Node "Body" (map stmtToTree body)
    ]

stmtToTree (For initStmt cond step body) =
    Node "For" [
        Node "Init" (maybeStmtToTree initStmt),
        Node "Condition" (maybeExprToTree cond),
        Node "Step" (maybeStmtToTree step),
        Node "Body" (map stmtToTree body)
    ]
  where
    maybeStmtToTree Nothing = []
    maybeStmtToTree (Just s) = [stmtToTree s]
    maybeExprToTree Nothing = []
    maybeExprToTree (Just e) = [exprToTree e]

stmtToTree (Return maybeExpr) =
    Node "Return" (case maybeExpr of 
                     Nothing -> []
                     Just e  -> [exprToTree e])

stmtToTree (ExprStmt expr) =
    Node "ExprStmt" [exprToTree expr]

-- =============================================================================
-- CONVERSORES PARA EXPRESSÕES
-- =============================================================================

exprToTree :: Expr -> Tree String
-- Literais (Folhas da árvore)
exprToTree (LitInt i) = Node ("LitInt: " ++ show i) []
exprToTree (LitFloat f) = Node ("LitFloat: " ++ show f) []
exprToTree (LitBool b) = Node ("LitBool: " ++ show b) []
exprToTree (LitString s) = Node ("LitString: " ++ show s) []
exprToTree (Var s) = Node ("Var: " ++ s) []

-- Estruturas Complexas
exprToTree (ArrayLit exprs) = 
    Node "ArrayLit" (map exprToTree exprs)

exprToTree (Binary op e1 e2) =
    Node ("Binary " ++ show op) [exprToTree e1, exprToTree e2]

exprToTree (Unary op e) =
    Node ("Unary " ++ show op) [exprToTree e]

exprToTree (ArrayAccess arr idx) =
    Node "ArrayAccess" [
        Node "Array" [exprToTree arr],
        Node "Index" [exprToTree idx]
    ]

exprToTree (FieldAccess obj field) =
    Node ("FieldAccess ." ++ field) [exprToTree obj]

exprToTree (Call name args) =
    Node ("Call: " ++ name) (map exprToTree args)

exprToTree (New typ size) =
    Node ("New " ++ show typ) [exprToTree size]