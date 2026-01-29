module AST where

-- =============================================================================
-- TIPOS DE DADOS (Types)
-- =============================================================================

data Type
    = TyInt
    | TyFloat
    | TyBool
    | TyString
    | TyVoid
    | TyCustom String       -- Ex: Person (nome de struct)
    | TyArray Type          -- Ex: int[]
    | TyVar String          -- Ex: 'a' em generics (forall a)
    | TyFunc [Type] Type  -- <--- NOVO: Tipo função (Lista de Args -> Retorno)
    deriving (Eq, Show)

-- =============================================================================
-- OPERADORES
-- =============================================================================

data BinOp
    = Add | Sub | Mul | Div | Mod    -- Aritméticos
    | Eq  | Neq | Lt  | Gt  | Le | Ge -- Comparação
    | And | Or                        -- Lógicos
    deriving (Eq, Show)

data UnOp
    = Neg   -- -x (Menos unário)
    | Not   -- !x (Negação lógica)
    deriving (Eq, Show)

-- =============================================================================
-- EXPRESSÕES (Expressions) - Coisas que retornam valor
-- =============================================================================

data Expr
    -- Literais
    = LitInt Integer
    | LitFloat Double
    | LitBool Bool
    | LitString String
    
    -- Acesso a Variáveis e Estruturas
    | Var String                    -- x
    | ArrayAccess Expr Expr         -- arr[index]
    | ArrayLit [Expr]    -- [1, 2, 3]
    | FieldAccess Expr String       -- obj.field
    
    -- Chamada de Função
    | Call String [Expr]            -- func(arg1, arg2)
    
    -- Operações
    | Binary BinOp Expr Expr        -- a + b
    | Unary UnOp Expr               -- !a
    
    -- Alocação (Arrays)
    | New Type Expr                 -- new int[size]
    deriving (Eq, Show)

-- =============================================================================
-- DECLARAÇÕES / COMANDOS (Statements) - Ações
-- =============================================================================

data Stmt
    -- Declaração de Variável: let x int = 10;
    -- (Nome, Tipo, Inicialização Opcional)
    = VarDecl String Type (Maybe Expr)
    
    -- Atribuição: x = 10; ou arr[0] = 5;
    | Assign Expr Expr
    
    -- Estruturas de Controle
    | If Expr Block (Maybe Block)   -- if (cond) { ... } else { ... }
    | While Expr Block              -- while (cond) { ... }
    | For (Maybe Stmt) (Maybe Expr) (Maybe Stmt) Block -- for(init; cond; step)
    
    -- Retorno
    | Return (Maybe Expr)
    
    -- Expressão como instrução (ex: chamar função void)
    | ExprStmt Expr
    deriving (Eq, Show)

-- Um Bloco é apenas uma lista de Statements
type Block = [Stmt]

-- =============================================================================
-- DEFINIÇÕES DE TOPO (Top Level Definitions)
-- =============================================================================

data TopDecl
    -- Função: nome, generics, params, tipo_retorno, corpo
    -- Ex: forall a. func map(v: a[]) : a[] { ... }
    = FuncDecl {
        funcName :: String,
        funcGenerics :: [String],        -- Lista de variáveis genéricas (ex: ["a", "b"])
        funcParams :: [(String, Type)],  -- Lista de (Nome, Tipo)
        funcRetType :: Type,
        funcBody :: Block
      }
      
    -- Struct: nome, campos
    -- Ex: struct Person { name string; age int; }
    | StructDecl {
        structName :: String,
        structFields :: [(String, Type)] -- Lista de (Nome, Tipo)
      }
    deriving (Eq, Show)

-- O Programa inteiro é uma lista de declarações de topo
type Program = [TopDecl]
