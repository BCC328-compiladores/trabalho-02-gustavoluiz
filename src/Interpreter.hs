module Interpreter where

import AST
import qualified Data.Map as Map
import Control.Monad.State
-- O import explícito de liftIO foi removido pois StateT já lida com isso
import Control.Monad (zipWithM_) 

-- =============================================================================
-- 1. DEFINIÇÃO DE VALORES (Runtime Values)
-- =============================================================================

data Value
    = VInt Integer
    | VFloat Double
    | VBool Bool
    | VString String
    | VVoid
    | VArray [Value]              -- Array é uma lista de valores
    | VStruct String (Map.Map String Value) -- Nome da struct e mapa de campos
    | VNull                       -- Para inicialização segura
    deriving (Show, Eq)

-- =============================================================================
-- 2. AMBIENTE DE EXECUÇÃO (Memory)
-- =============================================================================

type VarTable = Map.Map String Value  
type FuncTable = Map.Map String TopDecl 

data Env = Env {
    scopes :: [VarTable],    -- Pilha de escopos (o topo é o head da lista)
    funcs  :: FuncTable      -- Funções definidas no programa
}

-- Inicializa o ambiente
initEnv :: [TopDecl] -> Env
initEnv decls = Env {
    scopes = [Map.empty], 
    funcs  = Map.fromList [ (getName d, d) | d <- decls, isFunc d ]
}
  where
    getName (FuncDecl name _ _ _ _) = name
    getName _ = ""
    isFunc (FuncDecl {}) = True
    isFunc _ = False

type Interp a = StateT Env IO a

data StmtResult
    = Next           
    | Ret Value      

-- =============================================================================
-- 3. HELPERS DE ESCOPO E UTILITÁRIOS
-- =============================================================================

pushScope :: Interp ()
pushScope = modify $ \env -> env { scopes = Map.empty : scopes env }

popScope :: Interp ()
popScope = modify $ \env -> env { scopes = tail (scopes env) }

-- Declara variável (Versão Segura)
declareVar :: String -> Value -> Interp ()
declareVar name val = modify $ \env ->
    case scopes env of
        (top:rest) -> env { scopes = Map.insert name val top : rest }
        []         -> error "Erro Fatal: Tentativa de declarar variavel sem escopo ativo!"

-- Atualiza variável
updateVar :: String -> Value -> Interp ()
updateVar name val = do
    env <- get
    let newScopes = updateInStack (scopes env)
    put $ env { scopes = newScopes }
  where
    updateInStack [] = error $ "Erro interno: Variavel nao encontrada no runtime: " ++ name
    updateInStack (s:ss)
        | Map.member name s = Map.insert name val s : ss
        | otherwise         = s : updateInStack ss

getVar :: String -> Interp Value
getVar name = do
    env <- get
    lookInStack (scopes env)
  where
    lookInStack [] = error $ "Erro interno: Variavel nao encontrada no runtime: " ++ name
    lookInStack (s:ss) = case Map.lookup name s of
        Just val -> return val
        Nothing  -> lookInStack ss

-- Helper para atualizar lista em índice específico (para Arrays)
updateList :: [a] -> Int -> a -> [a]
updateList [] _ _ = [] 
updateList (_:xs) 0 val = val : xs -- Correção: usamos _ pois o valor antigo não importa
updateList (x:xs) i val = x : updateList xs (i - 1) val

-- Helper para valor padrão na inicialização (New)
defaultVal :: Type -> Value
defaultVal TyInt = VInt 0
defaultVal TyFloat = VFloat 0.0
defaultVal TyBool = VBool False
defaultVal TyString = VString ""
defaultVal (TyArray _) = VNull 
defaultVal (TyCustom _) = VNull
defaultVal TyVoid = VVoid
defaultVal _ = VNull -- Correção: Catch-all para TyVar, TyFunc, etc.

-- =============================================================================
-- 4. AVALIADOR DE EXPRESSÕES
-- =============================================================================

evalExpr :: Expr -> Interp Value
evalExpr expr = case expr of
    LitInt i    -> return $ VInt i
    LitFloat f  -> return $ VFloat f
    LitBool b   -> return $ VBool b
    LitString s -> return $ VString s
    
    Var name    -> getVar name

    Binary op e1 e2 -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        evalBinary op v1 v2

    -- Chamada de Função
    Call "print" args -> do
        vals <- mapM evalExpr args
        liftIO $ mapM_ printValue vals
        return VVoid

    Call name args -> do
        argVals <- mapM evalExpr args
        env <- get
        case Map.lookup name (funcs env) of
            Just (FuncDecl _ _ params _ block) -> do
                pushScope
                let paramNames = map fst params 
                zipWithM_ declareVar paramNames argVals
                result <- evalBlock block
                popScope
                case result of
                    Ret val -> return val
                    Next    -> return VVoid 
            _ -> error $ "Funcao nao encontrada: " ++ name

    -- Alocação (New) - Arrays e Structs
    New typeVar sizeExpr -> do 
        sizeVal <- evalExpr sizeExpr
        case (typeVar, sizeVal) of
            -- PRIORIDADE 1: Struct (new Ponto)
            -- Se for um tipo Custom E o tamanho for 0 (nosso hack do Parser), é uma Struct.
            (TyCustom sName, VInt 0) -> return $ VStruct sName Map.empty

            -- PRIORIDADE 2: Array (new int[10] ou new Ponto[5])
            -- Qualquer outro caso (primitive types ou tamanho > 0) é array.
            (_, VInt s) -> return $ VArray (replicate (fromIntegral s) (defaultVal typeVar))
            
            _ -> error "New com parametros invalidos"

    -- Acesso a Array (Leitura)
    ArrayAccess arr index -> do
        vArr <- evalExpr arr
        vIdx <- evalExpr index
        case (vArr, vIdx) of
            (VArray list, VInt i) -> 
                if i >= 0 && i < fromIntegral (length list)
                    then return (list !! fromIntegral i)
                    else error $ "Indice fora dos limites: " ++ show i
            _ -> error "Tentativa de indexar algo que nao eh array"

    -- Acesso a Struct (Leitura)
    FieldAccess recordExpr fieldName -> do
        val <- evalExpr recordExpr
        case val of
            VStruct _ fields -> case Map.lookup fieldName fields of
                Just v -> return v
                Nothing -> return (VInt 0) -- Valor padrão ou erro
            _ -> error "Acesso de campo em nao-struct"
            
    _ -> error $ "Expressao nao implementada no interpretador: " ++ show expr

-- Auxiliares
evalBinary :: BinOp -> Value -> Value -> Interp Value
evalBinary Add (VInt a) (VInt b) = return $ VInt (a + b)
evalBinary Sub (VInt a) (VInt b) = return $ VInt (a - b)
evalBinary Mul (VInt a) (VInt b) = return $ VInt (a * b)
evalBinary Div (VInt a) (VInt b) = return $ VInt (a `div` b)
evalBinary Eq  (VInt a) (VInt b) = return $ VBool (a == b)
evalBinary Lt  (VInt a) (VInt b) = return $ VBool (a < b)
evalBinary Gt  (VInt a) (VInt b) = return $ VBool (a > b)
evalBinary Le  (VInt a) (VInt b) = return $ VBool (a <= b)
evalBinary And (VBool a) (VBool b) = return $ VBool (a && b)
evalBinary Or  (VBool a) (VBool b) = return $ VBool (a || b)
evalBinary _ _ _ = error "Operacao invalida em tempo de execucao"

printValue :: Value -> IO ()
printValue (VInt i) = print i
printValue (VBool b) = print b
printValue (VString s) = putStrLn s
printValue v = print v

-- =============================================================================
-- 5. EXECUTOR DE STATEMENTS
-- =============================================================================

evalStmt :: Stmt -> Interp StmtResult
evalStmt stmt = case stmt of
    -- Declaração
    VarDecl name _ (Just expr) -> do
        val <- evalExpr expr
        declareVar name val
        return Next

    VarDecl name _ Nothing -> do
        declareVar name VNull 
        return Next

    -- Atribuição
    Assign target expr -> do
        newVal <- evalExpr expr
        case target of
            Var name -> updateVar name newVal
            
            ArrayAccess arrExpr indexExpr -> do
                case arrExpr of
                    Var arrName -> do
                        currentArr <- getVar arrName
                        idxVal <- evalExpr indexExpr
                        case (currentArr, idxVal) of
                            (VArray list, VInt i) -> do
                                let newList = updateList list (fromIntegral i) newVal
                                updateVar arrName (VArray newList)
                            _ -> error "Erro na atribuicao de array"
                    _ -> error "Atribuicao de array so suportada em variaveis diretas"

            FieldAccess recordExpr fieldName -> do
                case recordExpr of
                    Var varName -> do
                        currentStruct <- getVar varName
                        case currentStruct of
                            VStruct sName fields -> do
                                let newFields = Map.insert fieldName newVal fields
                                updateVar varName (VStruct sName newFields)
                            _ -> error "Tentativa de atribuir campo em nao-struct"
                    _ -> error "Atribuicao de struct so suportada em variaveis diretas"

            _ -> error "Atribuicao complexa nao implementada"
        return Next

    -- If / Else (CORRIGIDO COM ESCOPO)
    If cond blockTrue maybeBlockFalse -> do
        vCond <- evalExpr cond
        case vCond of
            VBool True -> evalScopedBlock blockTrue -- Usa escopo isolado
            VBool False -> case maybeBlockFalse of
                Just blockFalse -> evalScopedBlock blockFalse -- Usa escopo isolado
                Nothing -> return Next
            _ -> error "If condicao nao eh bool"

    -- While (CORRIGIDO COM ESCOPO)
    While cond block -> do
        vCond <- evalExpr cond
        case vCond of
            VBool True -> do
                -- Executa o corpo do loop em um escopo novo a cada iteração
                res <- evalScopedBlock block
                case res of
                    Ret v -> return (Ret v) 
                    Next  -> evalStmt (While cond block) 
            VBool False -> return Next
            _ -> error "While condicao nao eh bool"

    -- Return
    Return (Just expr) -> do
        val <- evalExpr expr
        return (Ret val)
    Return Nothing -> return (Ret VVoid)

    -- Expression Statement
    ExprStmt expr -> do
        _ <- evalExpr expr
        return Next

    _ -> return Next

-- Executa uma lista de statements
evalBlock :: [Stmt] -> Interp StmtResult
evalBlock [] = return Next
evalBlock (s:ss) = do
    res <- evalStmt s
    case res of
        Ret val -> return (Ret val) 
        Next    -> evalBlock ss

evalScopedBlock :: [Stmt] -> Interp StmtResult
evalScopedBlock stmts = do
    pushScope       -- 1. Cria nova camada na pilha
    res <- evalBlock stmts
    popScope        -- 2. Remove a camada (mesmo se houver Return)
    return res

-- =============================================================================
-- 6. PONTO DE ENTRADA
-- =============================================================================

interpret :: Program -> IO ()
interpret program = do
    let env = initEnv program
    case Map.lookup "main" (funcs env) of
        Just (FuncDecl _ _ _ _ block) -> do
            putStrLn "--- INICIANDO INTERPRETADOR ---"
            _ <- evalStateT (evalBlock block) env
            putStrLn "\n--- FIM DA EXECUCAO ---"
        _ -> putStrLn "Erro: Funcao main nao encontrada."