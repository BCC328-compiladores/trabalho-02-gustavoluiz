{
module Parser where

import Tokens
import AST
}

%name parse
%tokentype { Token }
%error { parseError }

-- =============================================================================
-- TOKENS DEFINITION
-- =============================================================================

%token
    func        { Tok _ TkFunc }
    return      { Tok _ TkReturn }
    let         { Tok _ TkLet }
    if          { Tok _ TkIf }
    else        { Tok _ TkElse }
    while       { Tok _ TkWhile }
    for         { Tok _ TkFor }
    struct      { Tok _ TkStruct }
    forall      { Tok _ TkForall }
    print       { Tok _ TkPrint }
    new         { Tok _ TkNew }
    
    int         { Tok _ TkTypeInt }
    float       { Tok _ TkTypeFloat }
    string      { Tok _ TkTypeString }
    bool        { Tok _ TkTypeBool }
    void        { Tok _ TkTypeVoid }
    
    true        { Tok _ TkTrue }
    false       { Tok _ TkFalse }
    id          { Tok _ (TkIdent $$) }
    intLit      { Tok _ (TkIntLit $$) }
    floatLit    { Tok _ (TkFloatLit $$) }
    stringLit   { Tok _ (TkStringLit $$) }
    
    '='         { Tok _ TkAssign }
    '+'         { Tok _ TkPlus }
    '-'         { Tok _ TkMinus }
    '*'         { Tok _ TkStar }
    '/'         { Tok _ TkSlash }
    '%'         { Tok _ TkPercent }
    
    '=='        { Tok _ TkEq }
    '!='        { Tok _ TkNeq }
    '<'         { Tok _ TkLT }
    '>'         { Tok _ TkGT }
    '<='        { Tok _ TkLe }
    '>='        { Tok _ TkGe }
    
    '&&'        { Tok _ TkAnd }
    '||'        { Tok _ TkOr }
    '!'         { Tok _ TkBang }
    
    '->'        { Tok _ TkArrow }
    '++'        { Tok _ TkInc }
    
    '('         { Tok _ TkLParen }
    ')'         { Tok _ TkRParen }
    '{'         { Tok _ TkLBrace }
    '}'         { Tok _ TkRBrace }
    '['         { Tok _ TkLBracket }
    ']'         { Tok _ TkRBracket }
    
    ';'         { Tok _ TkSemicolon }
    ','         { Tok _ TkComma }
    '.'         { Tok _ TkDot }
    ':'         { Tok _ TkColon }

-- =============================================================================
-- OPERATOR PRECEDENCE
-- =============================================================================

%right '->'   -- baixa prioridade, associativadade direita
%right '='
%left '||'
%left '&&'
%nonassoc '==' '!=' '<' '>' '<=' '>='
%left '+' '-'
%left '*' '/' '%'
%right '!' NEG
%left '.' '['

-- =============================================================================
-- GRAMMAR RULES
-- =============================================================================

%%

Program : TopDecls                  { $1 }

TopDecls : TopDecl TopDecls         { $1 : $2 }
         | {- empty -}              { [] }

TopDecl : FuncDecl                  { $1 }
        | StructDecl                { $1 }

-- Structs
StructDecl : struct id '{' StructFields '}' 
           { StructDecl $2 $4 }

StructFields : StructField StructFields  { $1 : $2 }
             | {- empty -}               { [] }

-- Supports both 'name string;' and 'age: int;'
StructField : id Type ';'                { ($1, $2) }
            | id ':' Type ';'            { ($1, $3) }

-- Functions
FuncDecl : func id '(' Params ')' ':' Type Block 
         { FuncDecl $2 [] $4 $7 $8 }
         
         | forall GenVars func id '(' Params ')' ':' Type Block
         { FuncDecl $4 $2 $6 $9 $10 }

GenVars : id GenVars    { $1 : $2 }
        | id            { [$1] }

Params : ParamList      { $1 }
       | {- empty -}    { [] }

ParamList : Param ',' ParamList  { $1 : $3 }
          | Param                { [$1] }

-- Supports both 'x int' and 'x: int'
Param : id Type         { ($1, $2) }
      | id ':' Type     { ($1, $3) }

-- Types
Type : int              { TyInt }
     | float            { TyFloat }
     | string           { TyString }
     | bool             { TyBool }
     | void             { TyVoid }
     | id               { TyCustom $1 }
     | Type '[' ']'     { TyArray $1 }
     -- Function Type: (int, bool) -> void
     | '(' TypeList ')' '->' Type  { TyFunc $2 $5 }

TypeList : Type ',' TypeList    { $1 : $3 }
         | Type                 { [$1] }
         | {- empty -}          { [] }

-- Blocks & Statements
Block : '{' Stmts '}'   { $2 }

Stmts : Stmt Stmts      { $1 : $2 }
      | {- empty -}     { [] }

Stmt : let id Type '=' Expr ';'           { VarDecl $2 $3 (Just $5) }
     | let id ':' Type '=' Expr ';'       { VarDecl $2 $4 (Just $6) } -- NOVA REGRA
     | let id Type ';'                    { VarDecl $2 $3 Nothing }
     | let id ':' Type ';'                { VarDecl $2 $4 Nothing }   -- NOVA REGRA
     -- Array sugar: let x int[10]; -> new int[10]
     | let id Type '[' intLit ']' ';'     { VarDecl $2 (TyArray $3) (Just (New $3 (LitInt $5))) }
     -- Array sugar with init: let x int[3] = [...];
     | let id Type '[' intLit ']' '=' Expr ';' { VarDecl $2 (TyArray $3) (Just $8) }
     
     | Expr '=' Expr ';'                  { Assign $1 $3 } 
     | if '(' Expr ')' Block else Block   { If $3 $5 (Just $7) }
     | if '(' Expr ')' Block              { If $3 $5 Nothing }
     | while '(' Expr ')' Block           { While $3 $5 }
     
     -- FOR Loop with special Step rule (no semicolon at end)
     | for '(' OptStmt OptExpr ';' ForStep ')' Block { For $3 $4 $6 $8 }
     
     | print '(' Expr ')' ';'             { ExprStmt (Call "print" [$3]) } 
     | return Expr ';'                    { Return (Just $2) }
     | return ';'                         { Return Nothing }
     | Expr ';'                           { ExprStmt $1 }

-- Helpers for FOR
OptStmt : Stmt          { Just $1 }
        | ';'           { Nothing }

OptExpr : Expr          { Just $1 }
        | {- empty -}   { Nothing }

-- ForStep: Increment (i++) or Assignment (i = i + 1) without semicolon
ForStep : id '++'       { Just (Assign (Var $1) (Binary Add (Var $1) (LitInt 1))) }
        | id '=' Expr   { Just (Assign (Var $1) $3) }
        | {- empty -}   { Nothing }

-- Expressions
Expr : intLit           { LitInt $1 }
     | floatLit         { LitFloat $1 }
     | stringLit        { LitString $1 }
     | true             { LitBool True }
     | false            { LitBool False }
     | id               { Var $1 }
     | '[' Args ']'     { ArrayLit $2 }
     
     | Expr '+' Expr    { Binary Add $1 $3 }
     | Expr '-' Expr    { Binary Sub $1 $3 }
     | Expr '*' Expr    { Binary Mul $1 $3 }
     | Expr '/' Expr    { Binary Div $1 $3 }
     | Expr '%' Expr    { Binary Mod $1 $3 }
     | Expr '&&' Expr   { Binary And $1 $3 }
     | Expr '||' Expr   { Binary Or $1 $3 }
     | Expr '==' Expr   { Binary Eq $1 $3 }
     | Expr '!=' Expr   { Binary Neq $1 $3 }
     | Expr '<' Expr    { Binary Lt $1 $3 }
     | Expr '>' Expr    { Binary Gt $1 $3 }
     | Expr '<=' Expr   { Binary Le $1 $3 }
     | Expr '>=' Expr   { Binary Ge $1 $3 }
     
     | '-' Expr %prec NEG  { Unary Neg $2 }
     | '!' Expr            { Unary Not $2 }
     
     | Expr '[' Expr ']'   { ArrayAccess $1 $3 }
     | Expr '.' id         { FieldAccess $1 $3 }
     | id '(' Args ')'     { Call $1 $3 }
     | new Type '[' Expr ']' { New $2 $4 }
     | '(' Expr ')'        { $2 }

Args : ArgList          { $1 }
     | {- empty -}      { [] }

ArgList : Expr ',' ArgList { $1 : $3 }
        | Expr             { [$1] }

{
parseError :: [Token] -> a
parseError toks = error $ "Syntax error at tokens: " ++ show toks
}