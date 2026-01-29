{
{-# LANGUAGE CPP #-}
module Lexer where

import Data.Word (Word8)
import Data.Int  (Int64)
import Data.Char (ord)
import Data.Maybe (fromMaybe)

import Tokens
}

%wrapper "monadUserState"
%encoding "utf8"
%token "Token"

-- -----------------------------------------------------------------------------
-- MACROS
-- -----------------------------------------------------------------------------

$digit      = 0-9
$alpha      = [A-Za-z]
$alnum      = [A-Za-z0-9]
$hex        = [0-9A-Fa-f]

-- Caracteres de espaço (inclui BOM e NBSP para segurança)
$white      = [\x20\t\n\r\f\v\xa0\xfeff]

-- Caracteres imprimíveis
$printable  = [\x20-\x10ffff]

$identRest  = [A-Za-z0-9_']

@string     = \" ( $printable # \" )* \"
@id         = $alpha $identRest*
@float      = $digit+ \. $digit+

-- -----------------------------------------------------------------------------
-- REGRAS
-- -----------------------------------------------------------------------------

tokens :-

    -- 1. Espaços e Comentários de Linha (Apenas no modo <0>)
    <0> $white+                           ;
    <0> "//".* ;

    -- 2. Início de Comentário de Bloco
    -- Ao encontrar /* no modo código, muda para modo comentário
    <0> "/*"                              { begin_comment }

    -- 3. Palavras-Chave (Restritas ao modo <0>)
    <0> "func"                            { mkToken (token_kw TkFunc) }
    <0> "return"                          { mkToken (token_kw TkReturn) }
    <0> "let"                             { mkToken (token_kw TkLet) }
    <0> "if"                              { mkToken (token_kw TkIf) }
    <0> "else"                            { mkToken (token_kw TkElse) }
    <0> "while"                           { mkToken (token_kw TkWhile) }
    <0> "for"                             { mkToken (token_kw TkFor) }
    <0> "struct"                          { mkToken (token_kw TkStruct) }
    <0> "forall"                          { mkToken (token_kw TkForall) }
    <0> "print"                           { mkToken (token_kw TkPrint) }
    <0> "new"                             { mkToken (token_kw TkNew) }
    <0> "true"                            { mkToken (token_kw TkTrue) }
    <0> "false"                           { mkToken (token_kw TkFalse) }

    -- 4. Tipos Primitivos (Restritos ao modo <0>)
    <0> "int"                             { mkToken (token_kw TkTypeInt) }
    <0> "float"                           { mkToken (token_kw TkTypeFloat) }
    <0> "string"                          { mkToken (token_kw TkTypeString) }
    <0> "bool"                            { mkToken (token_kw TkTypeBool) }
    <0> "void"                            { mkToken (token_kw TkTypeVoid) }

    -- 5. Identificadores e Literais (Restritos ao modo <0>)
    <0> @id                               { mkToken token_ident }
    <0> @string                           { mkToken token_string }
    <0> @float                            { mkToken token_float }
    <0> $digit+                           { mkToken token_int }

    -- 6. Operadores Compostos (Restritos ao modo <0>)
    <0> "=="                    { mkToken (sym TkEq) }
    <0> "++"                              { mkToken (sym TkInc) }  -- <--- NOVA REGRA
    <0> "!="                    { mkToken (sym TkNeq) }
    <0> "<="                    { mkToken (sym TkLe) }
    <0> ">="                    { mkToken (sym TkGe) }
    <0> "&&"                    { mkToken (sym TkAnd) }
    <0> "||"                    { mkToken (sym TkOr) }
    <0> "->"                    { mkToken (sym TkArrow) }



    
    <0> "+"                     { mkToken (sym TkPlus) }
    <0> "-"                     { mkToken (sym TkMinus) }
    <0> "*"                     { mkToken (sym TkStar) }
    <0> "/"                     { mkToken (sym TkSlash) }
    <0> "="                     { mkToken (sym TkAssign) }

    <0> "<"                     { mkToken (sym TkLT) }
    <0> ">"                     { mkToken (sym TkGT) }
    <0> "!"                     { mkToken (sym TkBang) }

    <0> "("                     { mkToken (sym TkLParen) }
    <0> ")"                     { mkToken (sym TkRParen) }

    <0> "{"                     { mkToken (sym TkLBrace) }
    <0> "}"                     { mkToken (sym TkRBrace) }

    <0> "["                     { mkToken (sym TkLBracket) }
    <0> "]"                     { mkToken (sym TkRBracket) }

    <0> ";"                     { mkToken (sym TkSemicolon) }
    <0> ","                     { mkToken (sym TkComma) }
    <0> "."                     { mkToken (sym TkDot) }
    <0> ":"                     { mkToken (sym TkColon) }
    <0> "%"                     { mkToken (sym TkPercent) }


    -- 8. Catch-All para erros no código
    <0> .                                 { action_error }


-- -----------------------------------------------------------------------------
-- REGRAS DE COMENTÁRIOS (<comment>)
-- -----------------------------------------------------------------------------

    -- Aninhamento: Se vir "/*" dentro de um comentário, aumenta profundidade
    <comment> "/*"                        { begin_inc_comment }
    
    -- Fechamento: Se vir "*/", diminui profundidade (e pode voltar a <0>)
    <comment> "*/"                        { end_comment }
    
    -- Consumir conteúdo: Espaços e caracteres quaisquer são ignorados
    <comment> $white+                     { comment_continue }
    <comment> .                           { comment_continue }


{
----------------------------------------------------------
-- CÓDIGO HASKELL AUXILIAR
----------------------------------------------------------

alexPosnToTriple :: AlexPosn -> (Int, Int, Int)
alexPosnToTriple (AlexPn off line col) = (off, line, col)

data AlexUserState = AlexUserState
    { lexerCommentDepth :: Int }

alexInitUserState :: AlexUserState
alexInitUserState = AlexUserState { lexerCommentDepth = 0 }

mkToken :: (AlexInput -> Int -> Token) -> AlexInput -> Int -> Alex Token
mkToken f input len = return (f input len)

mkTokFromKind :: TokenKind -> AlexInput -> Int -> Token
mkTokFromKind kind (pos, _, _, _) _ =
    Tok (alexPosnToTriple pos) kind

token_kw :: TokenKind -> AlexInput -> Int -> Token
token_kw = mkTokFromKind

token_ident (pos, _, _, s) len =
    Tok (alexPosnToTriple pos) (TkIdent (take len s))

token_int (pos, _, _, s) len =
    Tok (alexPosnToTriple pos) (TkIntLit (read (take len s)))

token_float (pos, _, _, s) len =
    Tok (alexPosnToTriple pos) (TkFloatLit (read (take len s)))

token_string (pos, _, _, s) len =
    Tok (alexPosnToTriple pos) (TkStringLit (read (take len s)))

sym :: TokenKind -> AlexInput -> Int -> Token
sym kind (pos, _, _, _) _ = Tok (alexPosnToTriple pos) kind


----------------------------------------------------------
-- MANIPULAÇÃO DE ESTADOS DE COMENTÁRIO
----------------------------------------------------------

-- Entra no modo comentário (profundidade 1)
begin_comment :: AlexInput -> Int -> Alex Token
begin_comment input len = do
    ust <- alexGetUserState
    alexSetUserState ust { lexerCommentDepth = 1 }
    begin comment input len

-- Incrementa profundidade (aninhado)
begin_inc_comment :: AlexInput -> Int -> Alex Token
begin_inc_comment _ _ = do
    ust <- alexGetUserState
    alexSetUserState ust { lexerCommentDepth = lexerCommentDepth ust + 1 }
    alexMonadScan

-- Decrementa profundidade ou sai do modo comentário
end_comment :: AlexInput -> Int -> Alex Token
end_comment input len = do
    ust <- alexGetUserState
    let d = lexerCommentDepth ust - 1
    if d <= 0
        then do
            alexSetUserState ust { lexerCommentDepth = 0 }
            begin 0 input len -- Volta para o estado <0>
        else do
            alexSetUserState ust { lexerCommentDepth = d }
            alexMonadScan

-- Ignora conteúdo
comment_continue :: AlexInput -> Int -> Alex Token
comment_continue _ _ = alexMonadScan

-- Fim de Arquivo
alexEOF :: Alex Token
alexEOF = do
    -- 1. Obtém o "Start Code" atual (o estado em que o Lexer parou)
    startCode <- alexGetStartCode
    
    -- 2. Verifica se o estado é 'comment'
    -- (Nota: 'comment' é uma constante gerada pelo Alex por causa da regra <comment>)
    if startCode == comment
        then do
            -- Se estiver no meio de um comentário, é um erro!
            -- Podemos até pegar a profundidade para informar melhor
            ust <- alexGetUserState
            let depth = lexerCommentDepth ust
            alexError $ "Erro: Comentário multilinha não fechado ao final do arquivo (Profundidade: " ++ show depth ++ ")"
        else
            -- 3. Caso contrário, retorna o token de fim de arquivo normalmente
            return (Tok (0,0,0) TokEOF)

-- Tratamento de Erro Léxico
action_error :: AlexInput -> Int -> Alex Token
action_error (pos, _, _, str) _ = do
    let (AlexPn _ line col) = pos
    let charMsg = if null str 
                  then "End of File" 
                  else show (head str) ++ " (ASCII: " ++ show (ord (head str)) ++ ")"
    alexError $ "Erro Léxico: Caractere inesperado " ++ charMsg ++ 
                " na linha " ++ show line ++ ", coluna " ++ show col

----------------------------------------------------------
-- LEXER RUNNER
----------------------------------------------------------

collectTokens :: Alex [Token]
collectTokens = do
    t <- alexMonadScan
    case t of
        Tok _ TokEOF -> return []
        _            -> (t :) <$> collectTokens

runLexer :: String -> Either String [Token]
runLexer s = runAlex s go
  where
    go = do
      t <- alexMonadScan
      case t of
        Tok _ TokEOF -> pure [] -- parser espera uma lista vazia []
        _            -> (t :) <$> go


}