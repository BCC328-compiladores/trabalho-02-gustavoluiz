module CodeGen where

import AST
import Control.Monad.State

-- O estado agora guarda o código acumulado (linhas de instruções)
type Gen a = State String a

-- =============================================================================
-- 1. FUNÇÕES AUXILIARES DE GERAÇÃO
-- =============================================================================

-- Adiciona uma linha ao código final
emit :: String -> Gen ()
emit line = modify (\s -> s ++ line ++ "\n")

-- Gera o cabeçalho do módulo e as definições básicas
-- RENOMEADO DE VOLTA PARA genProgram PARA FUNCIONAR COM O Main.hs
genProgram :: Program -> String
genProgram prog = 
    let body = execState (genTopLevel prog) ""
    in "(module\n" ++
       "  (import \"env\" \"print\" (func $print (param i32)))\n" ++
       "  (memory $0 1)\n" ++
       "  (export \"memory\" (memory $0))\n" ++
       body ++
       ")"

genTopLevel :: Program -> Gen ()
genTopLevel [] = return ()
genTopLevel (decl:decls) = do
    case decl of
        FuncDecl name _ params retType body -> genFunc name params retType body
        _ -> return () -- Ignora Structs por enquanto
    genTopLevel decls

-- Gera uma função completa no formato aninhado
genFunc :: String -> [(String, Type)] -> Type -> Block -> Gen ()
genFunc name params retType body = do
    -- Assinatura da função
    let paramStr = unwords $ map (\(n, _) -> "(param $" ++ n ++ " i32)") params
    let resultStr = if retType == TyVoid then "" else "(result i32)"
    
    emit $ "  (func $" ++ name ++ " " ++ paramStr ++ " " ++ resultStr
    
    -- Declarações locais
    mapM_ genLocalDecl body

    -- Gera o corpo da função
    mapM_ genStmt body
    
    -- Se for main, exporta
    emit "  )"
    if name == "main" 
        then emit $ "  (export \"main\" (func $" ++ name ++ "))"
        else return ()

-- Gera apenas a declaração de variáveis locais
genLocalDecl :: Stmt -> Gen ()
genLocalDecl (VarDecl name _ _) = emit $ "    (local $" ++ name ++ " i32)"
genLocalDecl (If _ b1 b2) = do
    mapM_ genLocalDecl b1
    case b2 of
        Just b -> mapM_ genLocalDecl b
        Nothing -> return ()
genLocalDecl (While _ b) = mapM_ genLocalDecl b
genLocalDecl _ = return ()

-- =============================================================================
-- 2. GERAÇÃO DE STATEMENTS (COM EXPRESSÕES ANINHADAS)
-- =============================================================================

genStmt :: Stmt -> Gen ()
genStmt stmt = case stmt of
    VarDecl name _ (Just expr) -> do
        eStr <- genExpr expr
        emit $ "    (local.set $" ++ name ++ " " ++ eStr ++ ")"
    
    -- CORRIGIDO: Substituído 'name' por '_' para evitar warning
    VarDecl _ _ Nothing -> 
        return () 

    Assign (Var name) expr -> do
        eStr <- genExpr expr
        emit $ "    (local.set $" ++ name ++ " " ++ eStr ++ ")"

    -- If no formato S-Expression
    If cond thenBlock elseBlock -> do
        condStr <- genExpr cond
        emit $ "    (if " ++ condStr
        emit "      (then"
        mapM_ genStmt thenBlock
        emit "      )"
        case elseBlock of
            Just block -> do
                emit "      (else"
                mapM_ genStmt block
                emit "      )"
            Nothing -> return ()
        emit "    )"

    -- While: (block $break (loop $top ...))
    While cond block -> do
        condStr <- genExpr cond
        emit "    (block $break"
        emit "      (loop $top"
        emit $ "        (br_if $break (i32.eqz " ++ condStr ++ "))"
        mapM_ genStmt block
        emit "        (br $top)"
        emit "      )"
        emit "    )"

    Return (Just expr) -> do
        eStr <- genExpr expr
        emit $ "    (return " ++ eStr ++ ")"
    
    Return Nothing -> 
        emit "    (return)"

    ExprStmt (Call "print" args) -> do
        argStrs <- mapM genExpr args
        mapM_ (\s -> emit $ "    (call $print " ++ s ++ ")") argStrs

    ExprStmt expr -> do
        eStr <- genExpr expr
        emit $ "    (drop " ++ eStr ++ ")"

    _ -> return ()

-- =============================================================================
-- 3. GERAÇÃO DE EXPRESSÕES (RETORNA STRING)
-- =============================================================================

genExpr :: Expr -> Gen String
genExpr expr = case expr of
    LitInt n -> return $ "(i32.const " ++ show n ++ ")"
    LitBool b -> return $ "(i32.const " ++ (if b then "1" else "0") ++ ")"
    
    Var name -> return $ "(local.get $" ++ name ++ ")"
    
    Binary op e1 e2 -> do
        s1 <- genExpr e1
        s2 <- genExpr e2
        let opStr = opToWasm op
        return $ "(" ++ opStr ++ " " ++ s1 ++ " " ++ s2 ++ ")"
    
    Unary Neg e -> do
        s <- genExpr e
        return $ "(i32.sub (i32.const 0) " ++ s ++ ")"
    
    Unary Not e -> do
        s <- genExpr e
        return $ "(i32.eqz " ++ s ++ ")"

    Call name args -> do
        argStrs <- mapM genExpr args
        let argsJoined = unwords argStrs
        return $ "(call $" ++ name ++ " " ++ argsJoined ++ ")"

    _ -> return "(i32.const 0)"

opToWasm :: BinOp -> String
opToWasm op = case op of
    Add -> "i32.add"
    Sub -> "i32.sub"
    Mul -> "i32.mul"
    Div -> "i32.div_s"
    Eq  -> "i32.eq"
    Neq -> "i32.ne"
    Lt  -> "i32.lt_s"
    Gt  -> "i32.gt_s"
    Le  -> "i32.le_s"
    Ge  -> "i32.ge_s"
    And -> "i32.and"
    Or  -> "i32.or"
    Mod -> "i32.rem_s"