module Interpreter where

import AST
import qualified Data.Map as Map
import Control.Monad.State
import Control.Monad (zipWithM_) 

-- =============================================================================
-- 1. DEFINIÇÃO DE VALORES (Runtime Values)
-- =============================================================================

-- [EXPLICACAO]:
-- Diferente do "TypeChecker" que lida com tipos abstratos (TyInt, TyBool),
-- o Interpretador lida com DADOS CONCRETOS.
-- Este ADT (Algebraic Data Type) é um "envelope" que permite guardar
-- qualquer valor da linguagem SL (número, string, array) em uma variável Haskell.
data Value
    = VInt Integer
    | VFloat Double
    | VBool Bool
    | VString String
    | VVoid
    | VArray [Value]              -- [DIDATICO]: Recursividade! Um array contém uma lista de Values.
    | VStruct String (Map.Map String Value) -- Struct real: Nome do tipo + Mapa de campos (nome -> valor)
    | VNull                       -- Usado para inicialização segura antes da atribuição.
    deriving (Show, Eq)

-- =============================================================================
-- 2. AMBIENTE DE EXECUÇÃO (Memory)
-- =============================================================================

type VarTable = Map.Map String Value  -- Tabela de Símbolos local (Nome -> Valor)
type FuncTable = Map.Map String TopDecl 

-- [EXPLICACAO DA ARQUITETURA]:
-- A memória é modelada como uma PILHA DE ESCOPOS (Stack of Scopes).
-- 'scopes' é uma lista: [EscopoAtual, EscopoPai, ..., EscopoGlobal]
-- Quando procuramos uma variável, buscamos do topo (head) para o fundo.
data Env = Env {
    scopes :: [VarTable],    
    funcs  :: FuncTable      
}

-- Inicializa o ambiente com um escopo global vazio e carrega as funções.
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

-- [MONAD STACK]:
-- StateT Env: Permite ler e alterar a memória (variáveis) durante a execução.
-- IO: Permite fazer input/output real (printar na tela do terminal).
type Interp a = StateT Env IO a

-- Define o resultado da execução de uma linha de código.
-- Next: "Continue para a próxima linha".
-- Ret: "Pare tudo e retorne este valor" (usado para o comando 'return').
data StmtResult
    = Next            
    | Ret Value       

-- =============================================================================
-- 3. HELPERS DE ESCOPO E UTILITÁRIOS
-- =============================================================================

-- [GESTAO DE MEMORIA]:
-- Entrar num bloco = Empilhar um mapa vazio no topo da lista.
pushScope :: Interp ()
pushScope = modify $ \env -> env { scopes = Map.empty : scopes env }

-- Sair de um bloco = Jogar fora o mapa do topo.
-- [IMPORTANTE]: Isso deleta logicamente todas as variáveis criadas dentro do bloco.
popScope :: Interp ()
popScope = modify $ \env -> env { scopes = tail (scopes env) }

-- Cria uma nova variável APENAS no escopo atual (topo da pilha).
declareVar :: String -> Value -> Interp ()
declareVar name val = modify $ \env ->
    case scopes env of
        (top:rest) -> env { scopes = Map.insert name val top : rest }
        []         -> error "Erro Fatal: Tentativa de declarar variavel sem escopo ativo!"

-- Atualiza uma variável existente.
-- Precisa procurar em qual escopo ela está (no atual ou num pai).
updateVar :: String -> Value -> Interp ()
updateVar name val = do
    env <- get
    let newScopes = updateInStack (scopes env)
    put $ env { scopes = newScopes }
  where
    updateInStack [] = error $ "Erro interno: Variavel nao encontrada no runtime: " ++ name
    updateInStack (s:ss)
        | Map.member name s = Map.insert name val s : ss -- Achou! Atualiza aqui.
        | otherwise         = s : updateInStack ss       -- Não achou, procura no pai (recursão).

-- Busca o valor de uma variável varrendo a pilha do topo para baixo.
getVar :: String -> Interp Value
getVar name = do
    env <- get
    lookInStack (scopes env)
  where
    lookInStack [] = error $ "Erro interno: Variavel nao encontrada no runtime: " ++ name
    lookInStack (s:ss) = case Map.lookup name s of
        Just val -> return val
        Nothing  -> lookInStack ss

-- Utilitários para manipular listas e valores padrão...
updateList :: [a] -> Int -> a -> [a]
updateList [] _ _ = [] 
updateList (_:xs) 0 val = val : xs 
updateList (x:xs) i val = x : updateList xs (i - 1) val

defaultVal :: Type -> Value
defaultVal TyInt = VInt 0
defaultVal TyFloat = VFloat 0.0
defaultVal TyBool = VBool False
defaultVal TyString = VString ""
defaultVal (TyArray _) = VNull 
defaultVal (TyCustom _) = VNull
defaultVal TyVoid = VVoid
defaultVal TyAuto = VNull     
defaultVal (TyGeneric _) = VNull 
defaultVal _ = VNull 

-- =============================================================================
-- 4. AVALIADOR DE EXPRESSÕES
-- =============================================================================

-- [MOTOR DE CALCULO]:
-- Transforma um nó da AST (Expressão) em um Valor Real (Value).
-- Esta função não altera o estado, apenas lê e calcula.
evalExpr :: Expr -> Interp Value
evalExpr expr = case expr of
    LitInt i    -> return $ VInt i
    LitFloat f  -> return $ VFloat f
    LitBool b   -> return $ VBool b
    LitString s -> return $ VString s
    
    Var name    -> getVar name -- Busca o valor atual na memória

    -- Avaliação recursiva: avalia esq, avalia dir, e opera.
    Binary op e1 e2 -> do
        v1 <- evalExpr e1
        v2 <- evalExpr e2
        evalBinary op v1 v2

    -- Chamada de Função Nativa (Print)
    Call "print" args -> do
        vals <- mapM evalExpr args      -- 1. Calcula todos os argumentos
        liftIO $ mapM_ printValue vals  -- 2. Executa IO real no terminal
        return VVoid

    -- Chamada de Função Definida pelo Usuário
    Call name args -> do
        argVals <- mapM evalExpr args   -- 1. Avalia argumentos (Passagem por Valor)
        env <- get
        case Map.lookup name (funcs env) of
            Just (FuncDecl _ _ params _ block) -> do
                pushScope                       -- 2. Cria novo escopo para a função
                let paramNames = map fst params 
                zipWithM_ declareVar paramNames argVals -- 3. Declara parâmetros como variáveis locais
                result <- evalBlock block       -- 4. Executa o corpo da função
                popScope                        -- 5. Destroi o escopo (Limpeza)
                
                -- Tratamento do Retorno
                case result of
                    Ret val -> return val       -- Veio um valor de return? Devolve ele.
                    Next    -> return VVoid     -- Acabou a função sem return? Devolve Void.
            _ -> error $ "Funcao nao encontrada: " ++ name

    -- Instanciação de Memória (New)
    New typeVar sizeExpr -> do 
        sizeVal <- evalExpr sizeExpr
        case (typeVar, sizeVal) of
            (TyCustom sName, VInt 0) -> return $ VStruct sName Map.empty -- Cria Struct vazia
            (_, VInt s) -> return $ VArray (replicate (fromIntegral s) (defaultVal typeVar)) -- Cria Array preenchido com zeros/nulls
            _ -> error "New com parametros invalidos"

    -- [SEGURANCA - BOUNDS CHECKING]:
    ArrayAccess arr index -> do
        vArr <- evalExpr arr
        vIdx <- evalExpr index
        case (vArr, vIdx) of
            (VArray list, VInt i) ->  
                -- Verifica matematicamente se o índice é válido antes de acessar
                if i >= 0 && i < fromIntegral (length list) 
                    then return (list !! fromIntegral i)
                    else error $ "Indice fora dos limites: " ++ show i -- Erro amigável
            _ -> error "Tentativa de indexar algo que nao eh array"

    -- Acesso a campo de Struct (record.campo)
    FieldAccess recordExpr fieldName -> do
        val <- evalExpr recordExpr
        case val of
            VStruct _ fields -> case Map.lookup fieldName fields of
                Just v -> return v
                Nothing -> return (VInt 0) 
            _ -> error "Acesso de campo em nao-struct"
            
    _ -> error $ "Expressao nao implementada no interpretador: " ++ show expr

evalBinary :: BinOp -> Value -> Value -> Interp Value

--ARITMÉTICA INTEIRA
evalBinary Add (VInt a) (VInt b) = return $ VInt (a + b)
evalBinary Sub (VInt a) (VInt b) = return $ VInt (a - b)
evalBinary Mul (VInt a) (VInt b) = return $ VInt (a * b)
evalBinary Div (VInt a) (VInt b) = if b == 0 
                                   then error "Erro de execucao: Divisao por zero inteira"
                                   else return $ VInt (a `div` b)
evalBinary Mod (VInt a) (VInt b) = return $ VInt (a `mod` b)

--COMPARAÇÃO INTEIRA
evalBinary Eq  (VInt a) (VInt b) = return $ VBool (a == b)
evalBinary Neq (VInt a) (VInt b) = return $ VBool (a /= b)
evalBinary Lt  (VInt a) (VInt b) = return $ VBool (a < b)
evalBinary Gt  (VInt a) (VInt b) = return $ VBool (a > b)
evalBinary Le  (VInt a) (VInt b) = return $ VBool (a <= b)
evalBinary Ge  (VInt a) (VInt b) = return $ VBool (a >= b)

--ARITMÉTICA FLOAT
evalBinary Add (VFloat a) (VFloat b) = return $ VFloat (a + b)
evalBinary Sub (VFloat a) (VFloat b) = return $ VFloat (a - b)
evalBinary Mul (VFloat a) (VFloat b) = return $ VFloat (a * b)
evalBinary Div (VFloat a) (VFloat b) = return $ VFloat (a / b)

--COMPARAÇÃO FLOAT
evalBinary Eq  (VFloat a) (VFloat b) = return $ VBool (a == b)
evalBinary Neq (VFloat a) (VFloat b) = return $ VBool (a /= b)
evalBinary Lt  (VFloat a) (VFloat b) = return $ VBool (a < b)
evalBinary Gt  (VFloat a) (VFloat b) = return $ VBool (a > b)
evalBinary Le  (VFloat a) (VFloat b) = return $ VBool (a <= b)
evalBinary Ge  (VFloat a) (VFloat b) = return $ VBool (a >= b)

--LÓGICA BOOLEANA
evalBinary And (VBool a) (VBool b) = return $ VBool (a && b)
evalBinary Or  (VBool a) (VBool b) = return $ VBool (a || b)
evalBinary Eq  (VBool a) (VBool b) = return $ VBool (a == b)
evalBinary Neq (VBool a) (VBool b) = return $ VBool (a /= b)

--STRINGS 
evalBinary Add (VString a) (VString b) = return $ VString (a ++ b)
evalBinary Eq  (VString a) (VString b) = return $ VBool (a == b)
evalBinary Neq (VString a) (VString b) = return $ VBool (a /= b)

-- Se cair aqui, mostraremos EXATAMENTE o que deu errado.
-- Ex: "Gt entre VInt 30 e VFloat 25.0"
evalBinary op v1 v2 = error $ "Erro de Execucao: Operacao binaria nao implementada ou tipos incompativeis: " 
                            ++ show op ++ " entre " ++ show v1 ++ " e " ++ show v2

printValue :: Value -> IO ()
printValue (VInt i) = print i
printValue (VBool b) = print b
printValue (VString s) = putStrLn s
printValue v = print v

-- =============================================================================
-- 5. EXECUTOR DE STATEMENTS
-- =============================================================================

-- [MOTOR DE ESTADO]:
-- Executa comandos que MUDAM a memória ou o fluxo.
-- Retorna StmtResult para avisar se deve continuar (Next) ou parar (Ret).
evalStmt :: Stmt -> Interp StmtResult
evalStmt stmt = case stmt of
    
    -- 1. Declaração: Calcula valor inicial e cria na memória
    VarDecl name _ (Just expr) -> do
        val <- evalExpr expr
        declareVar name val
        return Next

    -- 2. Atribuição: Calcula novo valor e atualiza memória existente
    Assign target expr -> do
        newVal <- evalExpr expr
        case target of
            Var name -> updateVar name newVal -- Atualiza variável simples
            
            -- Atualização complexa de array (arr[i] = val)
            ArrayAccess arrExpr indexExpr -> do
                case arrExpr of
                    Var arrName -> do
                        currentArr <- getVar arrName
                        idxVal <- evalExpr indexExpr
                        case (currentArr, idxVal) of
                            (VArray list, VInt i) -> do
                                -- Recria a lista com o novo valor (Imutabilidade do Haskell simulando Mutabilidade)
                                let newList = updateList list (fromIntegral i) newVal
                                updateVar arrName (VArray newList)
                            _ -> error "Erro na atribuicao de array"
                    _ -> error "Atribuicao de array so suportada em variaveis diretas"
            
            -- Atualização de Struct omitida para brevidade...
            _ -> error "Atribuicao complexa nao implementada"
        return Next

    -- 3. IF / ELSE
    If cond blockTrue maybeBlockFalse -> do
        vCond <- evalExpr cond
        case vCond of
            VBool True -> evalScopedBlock blockTrue 
            VBool False -> case maybeBlockFalse of
                Just blockFalse -> evalScopedBlock blockFalse
                Nothing -> return Next
            _ -> error "If condicao nao eh bool"

    -- 4. WHILE (LOOP)
    -- confere a condição / se verdadadeira, executa o bloco e chama o while novamente com os valores atualizados
    While cond block -> do
        vCond <- evalExpr cond
        case vCond of
            VBool True -> do
                res <- evalScopedBlock block -- Executa o corpo do while
                case res of
                    Ret v -> return (Ret v)  -- Se encontrou um 'return' dentro do while, para tudo e sobe.
                    Next  -> evalStmt (While cond block) -- RECURSÃO: Chama o While de novo.
            VBool False -> return Next -- Condição falsa, sai do loop.
            _ -> error "While condicao nao eh bool"

    -- 5. RETURN
    -- Envelopa o valor num resultado 'Ret' para interromper a execução da função.
    Return (Just expr) -> do
        val <- evalExpr expr
        return (Ret val)
    Return Nothing -> return (Ret VVoid)

    -- Statement que é só uma expressão (ex: chamada de função void)
    ExprStmt expr -> do
        _ <- evalExpr expr
        return Next

    _ -> return Next

-- Executa uma lista de comandos em sequência.
-- Se um deles retornar 'Ret', para a sequência imediatamente.
evalBlock :: [Stmt] -> Interp StmtResult
evalBlock [] = return Next
evalBlock (s:ss) = do
    res <- evalStmt s
    case res of
        Ret val -> return (Ret val) -- Propaga o sinal de parada (return)
        Next    -> evalBlock ss     -- Continua para a próxima linha

-- Helper para executar um bloco criando e destruindo escopo automaticamente
evalScopedBlock :: [Stmt] -> Interp StmtResult
evalScopedBlock stmts = do
    pushScope        
    res <- evalBlock stmts
    popScope         
    return res

-- =============================================================================
-- 6. PONTO DE ENTRADA
-- =============================================================================

-- Função principal que liga o motor
interpret :: Program -> IO ()
interpret program = do
    let env = initEnv program
    -- Procura pela função "main"
    case Map.lookup "main" (funcs env) of
        Just (FuncDecl _ _ _ _ block) -> do
            putStrLn "--- INICIANDO INTERPRETADOR ---"
            _ <- evalStateT (evalBlock block) env -- Dispara a execução
            putStrLn "\n--- FIM DA EXECUCAO ---"
        _ -> putStrLn "Erro: Funcao main nao encontrada."