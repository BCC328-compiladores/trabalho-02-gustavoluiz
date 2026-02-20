{-# LANGUAGE FlexibleContexts #-}

module TypeChecker where

import AST
import Control.Monad (unless, zipWithM_, void, when, foldM)
import Control.Monad.State
import Control.Monad.Except
import qualified Data.Map as Map

-- =============================================================================
-- 1. ESTRUTURAS DE DADOS E AMBIENTE
-- =============================================================================

--Esta seção define a "memória" do compilador durante a verificação


-- Guarda os tipos genéricos definidos (ex: ["T"]) e a assinatura da função
data FuncScheme = FuncScheme {
    fsGenerics :: [String], -- Lista de variáveis de tipo (ex: T, U)
    fsType     :: Type      -- O tipo da função (TyFunc args ret)
} deriving (Show, Eq)

-- Um escopo de variáveis mapeia Nome -> Tipo
type VarScope = Map.Map String Type

data Env = Env {
    -- Pilha de escopos: head é o escopo local atual, last é o global
    varScopes :: [VarScope], 
    
    -- Funções globais: Agora mapeia para FuncScheme em vez de Type
    funcEnv   :: Map.Map String FuncScheme,

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

--Como o código roda.

-- define a monada de verificacao, ExceptT lida com erros e State Env Para manter e modificar o estado (Env) enquanto percorremos o código.
type Check a = ExceptT String (State Env) a


--runCheck: Função que executa a verificação, recebendo o estado inicial e retornando o resultado ou erro.
runCheck :: Check a -> Env -> (Either String a, Env)
runCheck check env = runState (runExceptT check) env

-- =============================================================================
-- 3. FUNÇÕES AUXILIARES DE ESCOPO
-- =============================================================================

--Funções para manipular a pilha de variáveis.


--lookupVar: Procura uma variável. Começa no escopo local (topo da pilha); se não achar, desce para os escopos anteriores até o global.
lookupVar :: String -> Check Type
lookupVar name = do
    env <- get
    findInScopes (varScopes env)
  where
    findInScopes [] = throwError $ "Variavel nao declarada: " ++ name
    findInScopes (scope:rest) = case Map.lookup name scope of
        Just t  -> return t
        Nothing -> findInScopes rest

--addVar: Adiciona uma variável no escopo atual. Dá erro se já existir nesse mesmo escopo.
addVar :: String -> Type -> Check ()
addVar name type_ = do
    env <- get
    case varScopes env of
        [] -> throwError "Erro interno: Sem escopo para adicionar variavel"
        (current:rest) -> 
            if Map.member name current
                then throwError $ "Variavel ja declarada neste escopo: " ++ name
                else put $ env { varScopes = Map.insert name type_ current : rest }

------------------------------

--enterScope / exitScope: Usado ao entrar/sair de blocos { ... }. enterScope coloca um mapa vazio no topo da pilha; 
--exitScope remove o topo (apagando as variáveis locais daquele bloco).

enterScope :: Check ()
enterScope = do
    env <- get
    put $ env { varScopes = Map.empty : varScopes env }

exitScope :: Check ()
exitScope = do
    env <- get
    case varScopes env of
        [] -> throwError "Erro interno: Tentativa de fechar escopo inexistente"
        (_:rest) -> put $ env { varScopes = rest }

-------------------------------

--lookupFunc / lookupStruct: Busca definições de funções e structs nos mapas globais. 

-- NOVO: Retorna o Esquema (Scheme) da função
lookupFunc :: String -> Check FuncScheme
lookupFunc name = do
    env <- get
    case Map.lookup name (funcEnv env) of
        Just fs -> return fs
        Nothing -> throwError $ "Funcao nao declarada: " ++ name

lookupStruct :: String -> Check [(String, Type)]
lookupStruct name = do
    env <- get
    case Map.lookup name (structEnv env) of
        Just fields -> return fields
        Nothing -> throwError $ "Struct nao definida: " ++ name

-- =============================================================================
-- 4. LÓGICA DE GENERICS - Cérebro do polimorfismo
-- =============================================================================

-- Substitui tipos genéricos por tipos concretos
-- Ex: instantiate (TyGeneric "T") [("T", TyInt)] -> TyInt
-- Exemplo: Se a função retorna T e descobrimos que T = Int, esta função transforma TyGeneric "T" em TyInt

-- forall T. T -> T
-- identidade(5)
-- → T = Int
-- { "T" -> TyInt }
-- TyFunc [TyInt] TyInt


instantiate :: Type -> Map.Map String Type -> Type
instantiate t mappings = case t of
    TyGeneric name -> case Map.lookup name mappings of
        Just concreteType -> concreteType
        Nothing -> t 
    TyArray inner -> TyArray (instantiate inner mappings)
    TyFunc args ret -> 
        TyFunc (map (`instantiate` mappings) args) (instantiate ret mappings)
    -- TyCustom pode ser um Generic disfarçado se o Parser não detectou
    TyCustom name -> case Map.lookup name mappings of
        Just concreteType -> concreteType
        Nothing -> t
    _ -> t




-- Funcao principal de Unificacao: Descobre quem e "T" olhando para os argumentos
-- Tenta descobrir ("inferir") quais são os tipos concretos baseando-se nos argumentos passados.
solveGenerics :: [String] -> [Type] -> [Type] -> Either String (Map.Map String Type)
solveGenerics generics paramTypes argTypes = 
    -- 1. Pareia (Zip) o tipo esperado com o recebido e percorre a lista acumulando o resultado no Mapa
    foldM solveOne Map.empty (zip paramTypes argTypes)
  where
    solveOne currentMap (expected, actual) = case expected of
        -- 2. Se o tipo esperado e um Generico (ex: "T"), tenta registrar o tipo Real (ex: Int) no mapa
        TyGeneric gName | gName `elem` generics -> updateMap gName actual currentMap
        TyCustom gName  | gName `elem` generics -> updateMap gName actual currentMap
        
        -- 3. Se for Array (ex: T[]), "descasca" o array e chama recursivamente para resolver o tipo de dentro
        TyArray innerExpected -> case actual of
            TyArray innerActual -> solveOne currentMap (innerExpected, innerActual)
            _ -> Left "Esperava Array mas obteve outro tipo"
            
        -- 4. Se for tipo fixo (Int, Bool), exige igualdade estrita. Se for diferente, e erro.
        _ -> if expected == actual 
             then Right currentMap 
             else Left $ "Tipo incompativel. Esperado " ++ show expected ++ ", dado " ++ show actual

    -- Funcao auxiliar para guardar a descoberta no mapa com seguranca
    updateMap gName actual mapping = case Map.lookup gName mapping of
        -- 5. Se "T" ja foi descoberto antes, verifica se o novo tipo e igual ao anterior (Consistencia)
        Just existing -> 
            if existing == actual 
                then Right mapping 
                else Left $ "Conflito de inferencia para generic " ++ gName 
                        ++ ": " ++ show existing ++ " vs " ++ show actual
        -- 6. Se e a primeira vez vendo "T", salva no mapa
        Nothing -> Right (Map.insert gName actual mapping)

-- =============================================================================
-- 5. VERIFICAÇÃO DE EXPRESSÕES
-- =============================================================================
-- Analisa expressões que retornam valores (contas, chamadas, variáveis).

checkExpr :: Expr -> Check Type
checkExpr expr = case expr of
    LitInt _    -> return TyInt
    LitFloat _  -> return TyFloat
    LitBool _   -> return TyBool
    LitString _ -> return TyString

    Var name -> lookupVar name

    Binary op e1 e2 -> checkBinary op e1 e2
    Unary op e -> checkUnary op e

    -- Chamada de Função 
    Call name args -> do
        if name == "print" 
            then do
                if null args 
                    then throwError "Print exige pelo menos um argumento"
                    else do 
                        mapM_ checkExpr args 
                        return TyVoid
            else do
                -- 1. Busca o esquema da função
                (FuncScheme generics typeFunc) <- lookupFunc name
                
                case typeFunc of
                    TyFunc paramTypes retType -> do
                        if length args /= length paramTypes
                            then throwError $ "Numero incorreto de argumentos para funcao '" ++ name ++ "'"
                            else do
                                -- 2. Calcula tipos dos argumentos passados
                                argTypes <- mapM checkExpr args
                                
                                -- 3. Se a função não tem generics, checagem simples
                                if null generics 
                                    then do
                                        zipWithM_ checkCompatibility paramTypes argTypes
                                        return retType
                                    else do
                                        -- 4. INFERÊNCIA DE GENERICS
                                        -- Tenta descobrir o que é T baseando-se nos argumentos
                                        let result = solveGenerics generics paramTypes argTypes
                                        
                                        case result of
                                            Left err -> throwError err
                                            Right mapTypes -> do
                                                -- 5. Substitui T pelo tipo real no retorno
                                                return $ instantiate retType mapTypes

                    _ -> throwError $ "'" ++ name ++ "' nao é uma funcao"

-- ArrayAccess: Verifica se o índice é Int e se o alvo é um Array.
    ArrayAccess arr index -> do
        arrType <- checkExpr arr
        indexType <- checkExpr index
        if indexType /= TyInt 
            then throwError "Indice do array deve ser do tipo Int"
            else case arrType of
                TyArray innerType -> return innerType
                _ -> throwError "Tentativa de indexar algo que nao eh um array"

--FieldAccess: Verifica se a struct existe e se o campo pertence a ela.
    FieldAccess recordExpr fieldName -> do
        typeExpr <- checkExpr recordExpr
        case typeExpr of
            TyCustom sName -> do
                fields <- lookupStruct sName
                case lookup fieldName fields of
                    Just typeField -> return typeField
                    Nothing -> throwError $ "Campo '" ++ fieldName ++ "' nao existe na struct '" ++ sName ++ "'"
            _ -> throwError "Tentativa de acessar campo em algo que nao eh uma struct"

-- New: Valida a alocação de arrays (o tamanho deve ser Int)
    New typeVar sizeExpr -> do
        typeSize <- checkExpr sizeExpr
        if typeSize /= TyInt
            then throwError "Tamanho/Dimensão deve ser do tipo Int"
            else case (typeVar, sizeExpr) of
                (TyCustom _, LitInt 0) -> return typeVar
                _ -> return (TyArray typeVar)

    _ -> throwError $ "Expressao ainda nao implementada ou invalida: " ++ show expr

-- Helper para verificar compatibilidade
checkCompatibility :: Type -> Type -> Check ()
checkCompatibility expected actual = 
    if expected == actual 
        then return () 
        else throwError $ "Esperava tipo " ++ show expected ++ ", mas obteve " ++ show actual

-- =============================================================================
-- 6. LÓGICA DE OPERADORES
-- =============================================================================
--Valida operações matemáticas e lógicas.

-- descobre o tipo dos operando , impede somar um numero com um booleano
checkBinary :: BinOp -> Expr -> Expr -> Check Type
checkBinary op e1 e2 = do
    t1 <- checkExpr e1
    t2 <- checkExpr e2
    case op of
        _ | op `elem` [Add, Sub, Mul, Div] -> 
            case (t1, t2) of
                (TyInt, TyInt)     -> return TyInt
                (TyFloat, TyFloat) -> return TyFloat
                (TyInt, TyFloat)   -> throwError "Nao pode operar Int com Float (sem cast)"
                (TyFloat, TyInt)   -> throwError "Nao pode operar Float com Int (sem cast)"
                _ -> throwError $ "Operacao aritmetica invalida entre " ++ show t1 ++ " e " ++ show t2
        Mod -> 
            if t1 == TyInt && t2 == TyInt 
                then return TyInt 
                else throwError "Operador Mod (%) so aceita inteiros"
        _ | op `elem` [Eq, Neq, Lt, Gt, Le, Ge] ->
            if t1 == t2 then return TyBool else throwError $ "Tipos diferentes: " ++ show t1 ++ " e " ++ show t2
        _ | op `elem` [And, Or] ->
            if t1 == TyBool && t2 == TyBool then return TyBool else throwError "Logica requer Bool"
        _ -> throwError $ "Operador invalido: " ++ show op

checkUnary :: UnOp -> Expr -> Check Type
checkUnary op e = do
    t <- checkExpr e
    case op of
        Neg -> if t `elem` [TyInt, TyFloat] then return t else throwError "Neg requer numero"
        Not -> if t == TyBool then return TyBool else throwError "Not requer Bool"

-- =============================================================================
-- 7. VERIFICAÇÃO DE STATEMENTS
-- =============================================================================
--Analisa comandos que executam ações (declarações, loops, ifs).

checkStmt :: Stmt -> Check ()
checkStmt stmt = case stmt of

    --VarDecl: Lida com declaração de variáveis. Se o tipo for auto (TyAuto), ele infere o tipo baseado na expressão de inicialização.
    VarDecl name typeDecl maybeExpr -> do
        finalType <- case maybeExpr of
            Just expr -> do
                typeExpr <- checkExpr expr -- primeiro checa a expressao para depois, com o resultado, inferir o resultado - tipo da declaracao
                case typeDecl of
                    TyAuto -> return typeExpr -- INFERÊNCIA (TyAuto foi definido na AST e reconhece um "tipo omitido")
                    _ -> do
                        checkCompatibility typeDecl typeExpr  
                        return typeDecl
            Nothing -> do
                when (typeDecl == TyAuto) $ throwError "Nao eh possivel inferir tipo sem inicializacao"
                return typeDecl
        addVar name finalType

    Assign dest expr -> do
        typeDest <- checkExpr dest
        typeExpr <- checkExpr expr
        checkCompatibility typeDest typeExpr

    If cond thenBlock maybeElseBlock -> do
        typeCond <- checkExpr cond
        unless (typeCond == TyBool) $ throwError "Condicao do IF deve ser Bool"
        checkBlockWithScope thenBlock
        case maybeElseBlock of
            Just elseBlock -> checkBlockWithScope elseBlock
            Nothing -> return ()

    While cond block -> do
        typeCond <- checkExpr cond
        unless (typeCond == TyBool) $ throwError "Condicao do WHILE deve ser Bool"
        checkBlockWithScope block

    For maybeInit maybeCond maybeStep block -> do
        enterScope
        case maybeInit of { Just s -> checkStmt s; Nothing -> return () }
        case maybeCond of 
            Just c -> do { t <- checkExpr c; unless (t == TyBool) $ throwError "For Cond Bool" }
            Nothing -> return ()
        case maybeStep of { Just s -> checkStmt s; Nothing -> return () }
        checkBlock block
        exitScope

    -- Return: Verifica se o valor retornado bate com o currentRetType armazenado no Env (o tipo de retorno da função onde estamos).
    Return maybeExpr -> do
        env <- get
        case currentRetType env of
            Nothing -> throwError "RETURN fora de contexto"
            Just expectedType -> case maybeExpr of
                Just expr -> do
                    actualType <- checkExpr expr
                    if expectedType == TyVoid
                        then throwError "Funcao void com retorno"
                        else checkCompatibility expectedType actualType
                Nothing -> 
                    if expectedType == TyVoid then return () else throwError $ "Esperava retorno: " ++ show expectedType

    ExprStmt expr -> void $ checkExpr expr

-- =============================================================================
-- 8. HELPERS E MAIN
-- =============================================================================

--checkDeclBody: Agora que todos os nomes são conhecidos, ele entra em cada função e verifica o corpo (o código dentro dela usando o passo 7 'checkStmt').

checkBlock :: Block -> Check ()
checkBlock [] = return ()
checkBlock (s:ss) = checkStmt s >> checkBlock ss

checkBlockWithScope :: Block -> Check ()
checkBlockWithScope block = enterScope >> checkBlock block >> exitScope

runTypeCheck :: Program -> Either String ()
runTypeCheck prog = case runCheck (checkProgram prog) emptyEnv of
    (Left err, _) -> Left err
    (Right _, _)  -> Right ()

checkProgram :: Program -> Check ()
checkProgram decls = do
    mapM_ collectDecl decls
    mapM_ checkDeclBody decls

--collectDecl: Percorre todo o arquivo e registra todas as Structs e Funções (seus nomes e assinaturas) no Env. 
--Isso é crucial para permitir que uma função chame outra que foi definida mais abaixo no código (forward declaration).

collectDecl :: TopDecl -> Check ()
collectDecl decl = case decl of
    StructDecl name fields -> do
        env <- get
        if Map.member name (structEnv env)
            then throwError $ "Struct redefinida: " ++ name
            else put $ env { structEnv = Map.insert name fields (structEnv env) }

    
    FuncDecl name generics params retType _ -> do
        env <- get
        if Map.member name (funcEnv env)
            then throwError $ "Funcao redefinida: " ++ name
            else do
                let paramTypes = map snd params
                let funcType = TyFunc paramTypes retType
                -- Cria o esquema com Generics + Assinatura
                let scheme = FuncScheme generics funcType
                put $ env { funcEnv = Map.insert name scheme (funcEnv env) }

checkDeclBody :: TopDecl -> Check ()
checkDeclBody decl = case decl of
    StructDecl _ _ -> return ()

    FuncDecl _ _ params retType body -> do
        modify (\env -> env { currentRetType = Just retType })
        enterScope
        mapM_ (\(pName, pType) -> addVar pName pType) params
        checkBlock body
        exitScope