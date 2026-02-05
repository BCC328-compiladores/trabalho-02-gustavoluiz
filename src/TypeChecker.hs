{-# LANGUAGE FlexibleContexts #-}

module TypeChecker where

import AST
import Control.Monad (unless, zipWithM_, void)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

-- =============================================================================
-- 1. AMBIENTE (TABELA DE SÍMBOLOS)
-- =============================================================================

-- Um escopo de variáveis mapeia Nome -> Tipo
type VarScope = Map.Map String Type

data Env = Env {
    -- Pilha de escopos: head é o escopo local atual, last é o global
    varScopes :: [VarScope], 
    
    -- Funções globais: Nome -> Tipo da Função (TyFunc args ret)
    funcEnv   :: Map.Map String Type,

    -- Structs: Nome -> Lista de (Campo, Tipo)
    structEnv :: Map.Map String [(String, Type)],

    -- Para verificar se o 'return' bate com a função atual
    currentRetType :: Maybe Type
} deriving (Show)

emptyEnv :: Env
emptyEnv = Env { 
    varScopes = [Map.empty], 
    funcEnv = Map.empty, 
    structEnv = Map.empty,
    currentRetType = Nothing
}

-- =============================================================================
-- 2. MÓNADA DE VERIFICAÇÃO
-- =============================================================================

-- Mantém o estado (Env) e lida com erros (String)
type Check a = ExceptT String (State Env) a

runCheck :: Check a -> Env -> (Either String a, Env)
runCheck check env = runState (runExceptT check) env

-- =============================================================================
-- 3. FUNÇÕES AUXILIARES DE ESCOPO
-- =============================================================================

-- Busca variável na pilha de escopos (do mais interno para o mais externo)
lookupVar :: String -> Check Type
lookupVar name = do
    env <- get
    findInScopes (varScopes env)
  where
    findInScopes [] = throwError $ "Variavel nao declarada: " ++ name
    findInScopes (scope:rest) = case Map.lookup name scope of
        Just t  -> return t
        Nothing -> findInScopes rest

-- Adiciona variável no escopo local (topo da pilha)
addVar :: String -> Type -> Check ()
addVar name type_ = do
    env <- get
    case varScopes env of
        [] -> throwError "Erro interno: Sem escopo para adicionar variavel"
        (current:rest) -> 
            if Map.member name current
                then throwError $ "Variavel ja declarada neste escopo: " ++ name
                else put $ env { varScopes = Map.insert name type_ current : rest }

-- Entra num novo escopo (empilha um mapa vazio)
enterScope :: Check ()
enterScope = do
    env <- get
    put $ env { varScopes = Map.empty : varScopes env }

-- Sai do escopo atual (desempilha)
exitScope :: Check ()
exitScope = do
    env <- get
    case varScopes env of
        [] -> throwError "Erro interno: Tentativa de fechar escopo inexistente"
        (_:rest) -> put $ env { varScopes = rest }

-- Busca função global
lookupFunc :: String -> Check Type
lookupFunc name = do
    env <- get
    case Map.lookup name (funcEnv env) of
        Just t -> return t
        Nothing -> throwError $ "Funcao nao declarada: " ++ name

-- Busca definição de struct
lookupStruct :: String -> Check [(String, Type)]
lookupStruct name = do
    env <- get
    case Map.lookup name (structEnv env) of
        Just fields -> return fields
        Nothing -> throwError $ "Struct nao definida: " ++ name

-- =============================================================================
-- 4. VERIFICAÇÃO DE EXPRESSÕES
-- =============================================================================

checkExpr :: Expr -> Check Type
checkExpr expr = case expr of
    -- Literais
    LitInt _    -> return TyInt
    LitFloat _  -> return TyFloat
    LitBool _   -> return TyBool
    LitString _ -> return TyString

    -- Variáveis
    Var name -> lookupVar name

    -- Operações Binárias
    Binary op e1 e2 -> checkBinary op e1 e2

    -- Operações Unárias
    Unary op e -> checkUnary op e

    -- Chamada de Função
    Call name args -> do
        -- TRATAMENTO ESPECIAL PARA PRINT
        if name == "print" 
            then do
                -- Opcional: Verificar se tem argumentos
                if null args 
                    then throwError "Print exige pelo menos um argumento"
                    else do 
                        -- Verifica se os argumentos são expressões válidas, mas ignora o tipo
                        mapM_ checkExpr args 
                        return TyVoid
            else do
                -- LÓGICA PADRÃO PARA OUTRAS FUNÇÕES
                funcType <- lookupFunc name
                case funcType of
                    TyFunc paramTypes retType -> do
                        if length args /= length paramTypes
                            then throwError $ "Numero incorreto de argumentos para funcao '" ++ name ++ "'"
                            else do
                                argTypes <- mapM checkExpr args
                                zipWithM_ checkCompatibility paramTypes argTypes
                                return retType
                    _ -> throwError $ "'" ++ name ++ "' nao é uma funcao"


    -- Acesso a Array: arr[index]
    ArrayAccess arr index -> do
        arrType <- checkExpr arr
        indexType <- checkExpr index
        
        -- O índice deve ser Int
        if indexType /= TyInt 
            then throwError "Indice do array deve ser do tipo Int"
            else case arrType of
                TyArray innerType -> return innerType
                _ -> throwError "Tentativa de indexar algo que nao eh um array"

    -- Acesso a Campo de Struct: obj.campo
    FieldAccess recordExpr fieldName -> do
        typeExpr <- checkExpr recordExpr
        case typeExpr of
            TyCustom sName -> do
                fields <- lookupStruct sName
                case lookup fieldName fields of
                    Just typeField -> return typeField
                    Nothing -> throwError $ "Campo '" ++ fieldName ++ "' nao existe na struct '" ++ sName ++ "'"
            _ -> throwError "Tentativa de acessar campo em algo que nao eh uma struct"

    -- Alocação (New): Arrays e Structs
    New typeVar sizeExpr -> do
        typeSize <- checkExpr sizeExpr

        if typeSize /= TyInt
            then throwError "Tamanho/Dimensão deve ser do tipo Int"
            else case (typeVar, sizeExpr) of
                -- 1. Instanciação de Struct (Ex: new Ponto)
                -- O Parser envia LitInt 0 como tamanho "dummy" para structs
                (TyCustom _, LitInt 0) -> return typeVar

                -- 2. Alocação de Array (Ex: new int[10] ou new Ponto[5])
                _ -> return (TyArray typeVar)

    -- CATCH-ALL (DEVE SER O ÚLTIMO SEMPRE)
    _ -> throwError $ "Expressao ainda nao implementada ou invalida: " ++ show expr


-- Helper para verificar compatibilidade
checkCompatibility :: Type -> Type -> Check ()
checkCompatibility expected actual = 
    if expected == actual 
        then return () 
        else throwError $ "Esperava tipo " ++ show expected ++ ", mas obteve " ++ show actual

-- =============================================================================
-- 5. LÓGICA DE OPERADORES
-- =============================================================================

checkBinary :: BinOp -> Expr -> Expr -> Check Type
checkBinary op e1 e2 = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    
    case op of
        -- Aritmética (Soma, Sub, Mult, Div)
        _ | op `elem` [Add, Sub, Mul, Div] -> 
            case (t1, t2) of
                (TyInt, TyInt)     -> return TyInt
                (TyFloat, TyFloat) -> return TyFloat
                (TyInt, TyFloat)   -> throwError "Nao pode operar Int com Float (sem cast)"
                (TyFloat, TyInt)   -> throwError "Nao pode operar Float com Int (sem cast)"
                _ -> throwError $ "Operacao aritmetica invalida entre " ++ show t1 ++ " e " ++ show t2

        -- Modulo (apenas inteiros)
        Mod -> 
            if t1 == TyInt && t2 == TyInt 
                then return TyInt 
                else throwError "Operador Mod (%) so aceita inteiros"

        -- Comparação (==, !=, <, >, etc) - Retorna Bool
        _ | op `elem` [Eq, Neq, Lt, Gt, Le, Ge] ->
            if t1 == t2 
                then return TyBool
                else throwError $ "Nao pode comparar tipos diferentes: " ++ show t1 ++ " e " ++ show t2
        
        -- Lógicos (&&, ||) - Apenas Bool
        _ | op `elem` [And, Or] ->
            if t1 == TyBool && t2 == TyBool
                then return TyBool
                else throwError "Operadores logicos exigem operandos Bool"
        
        -- CATCH-ALL: Satisfaz o compilador e pega qualquer operador esquecido
        _ -> throwError $ "Operador nao implementado ou invalido: " ++ show op

checkUnary :: UnOp -> Expr -> Check Type
checkUnary op e = do
    t <- checkExpr e
    case op of
        Neg -> if t `elem` [TyInt, TyFloat] 
               then return t 
               else throwError "Menos unario (-) requer numero"
        Not -> if t == TyBool 
               then return TyBool 
               else throwError "Negacao (!) requer Bool"

-- =============================================================================
-- 6. VERIFICAÇÃO DE STATEMENTS (COMANDOS)
-- =============================================================================

checkStmt :: Stmt -> Check ()
checkStmt stmt = case stmt of

    -- Declaração: let x: int = 10;
    VarDecl name typeDecl maybeExpr -> do
        case maybeExpr of
            Just expr -> do
                typeExpr <- checkExpr expr
                checkCompatibility typeDecl typeExpr
            Nothing -> return ()
        addVar name typeDecl

    -- Atribuição: x = 10;
    Assign dest expr -> do
        typeDest <- checkExpr dest
        typeExpr <- checkExpr expr
        checkCompatibility typeDest typeExpr

    -- Estrutura IF
    If cond thenBlock maybeElseBlock -> do
        typeCond <- checkExpr cond
        if typeCond /= TyBool
            then throwError "Condicao do IF deve ser do tipo Bool"
            else return ()
            
        checkBlockWithScope thenBlock
        
        case maybeElseBlock of
            Just elseBlock -> checkBlockWithScope elseBlock
            Nothing -> return ()

    -- Estrutura WHILE
    While cond block -> do
        typeCond <- checkExpr cond
        if typeCond /= TyBool
            then throwError "Condicao do WHILE deve ser do tipo Bool"
            else return ()
        checkBlockWithScope block

    -- Estrutura FOR
    For maybeInit maybeCond maybeStep block -> do
        enterScope 
        
        case maybeInit of
            Just s -> checkStmt s
            Nothing -> return ()
            
        case maybeCond of
            Just c -> do
                t <- checkExpr c
                unless (t == TyBool) $ throwError "Condicao do FOR deve ser Bool"
            Nothing -> return () 
            
        case maybeStep of
            Just s -> checkStmt s
            Nothing -> return ()
            
        checkBlock block 
        exitScope

    -- Retorno de Função
    Return maybeExpr -> do
        env <- get
        case currentRetType env of
            Nothing -> throwError "RETURN fora de contexto de funcao"
            Just expectedType -> case maybeExpr of
                Just expr -> do
                    actualType <- checkExpr expr
                    if expectedType == TyVoid
                        then throwError "Funcao void nao deve retornar valor"
                        else checkCompatibility expectedType actualType
                Nothing -> 
                    if expectedType == TyVoid
                        then return ()
                        else throwError $ "Retorno vazio invalido. Esperava: " ++ show expectedType

    -- Expressão solta
    ExprStmt expr -> do
        void $ checkExpr expr -- Ignora o resultado, apenas checa erros
        return ()

-- =============================================================================
-- 7. HELPERS PARA BLOCOS E PROGRAMA
-- =============================================================================

checkBlock :: Block -> Check ()
checkBlock [] = return ()
checkBlock (s:ss) = do
    checkStmt s
    checkBlock ss

checkBlockWithScope :: Block -> Check ()
checkBlockWithScope block = do
    enterScope
    checkBlock block
    exitScope

-- Função principal exportada
runTypeCheck :: Program -> Either String ()
runTypeCheck prog = 
    case runCheck (checkProgram prog) emptyEnv of
        (Left err, _) -> Left err
        (Right _, _)  -> Right ()

checkProgram :: Program -> Check ()
checkProgram decls = do
    -- 1ª Passagem: Coletar
    mapM_ collectDecl decls
    -- 2ª Passagem: Verificar corpos
    mapM_ checkDeclBody decls

collectDecl :: TopDecl -> Check ()
collectDecl decl = case decl of
    StructDecl name fields -> do
        env <- get
        if Map.member name (structEnv env)
            then throwError $ "Struct redefinida: " ++ name
            else put $ env { structEnv = Map.insert name fields (structEnv env) }

    FuncDecl name _ params retType _ -> do
        env <- get
        if Map.member name (funcEnv env)
            then throwError $ "Funcao redefinida: " ++ name
            else do
                let paramTypes = map snd params
                let funcType = TyFunc paramTypes retType
                put $ env { funcEnv = Map.insert name funcType (funcEnv env) }

checkDeclBody :: TopDecl -> Check ()
checkDeclBody decl = case decl of
    StructDecl _ _ -> return ()

    FuncDecl _ _ params retType body -> do
        modify (\env -> env { currentRetType = Just retType })
        enterScope
        mapM_ (\(pName, pType) -> addVar pName pType) params
        checkBlock body
        exitScope