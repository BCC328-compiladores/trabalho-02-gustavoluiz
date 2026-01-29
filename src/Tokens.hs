-- src/Tokens.hs
module Tokens
  ( Token(..)
  , TokenKind(..)
  , tokPosToTuple
  ) where

-- tokPos: (absoluteOffset, line, column)
data Token = Tok
  { tokPos  :: (Int, Int, Int)
  , tokKind :: TokenKind
  } deriving (Eq, Show)

data TokenKind
  -- Palavras-chave
  = TkFunc
  | TkReturn
  | TkLet
  | TkIf
  | TkElse
  | TkWhile
  | TkFor
  | TkStruct
  | TkForall
  | TkPrint
  | TkNew

  -- Tipos primitivos
  | TkTypeInt
  | TkTypeFloat
  | TkTypeString
  | TkTypeBool
  | TkTypeVoid

  -- Literais booleanos
  | TkTrue
  | TkFalse

  -- Identificadores e literais
  | TkIdent String
  | TkIntLit Integer
  | TkFloatLit Double
  | TkStringLit String

  -- Operadores compostos
  | TkEq        -- ==
  | TkNeq       -- !=
  | TkLe        -- <=
  | TkGe        -- >=
  | TkAnd       -- &&
  | TkOr        -- ||
  | TkArrow     -- ->
  | TkInc        -- ++ 

  -- Operadores simples
  | TkPlus      -- +
  | TkMinus     -- -
  | TkStar      -- *
  | TkSlash     -- /
  | TkAssign    -- =

  | TkLT        -- <
  | TkGT        -- >
  | TkBang      -- !

  -- Delimitadores
  | TkLParen    -- (
  | TkRParen    -- )
  | TkLBrace    -- {
  | TkRBrace    -- }
  | TkLBracket  -- [
  | TkRBracket  -- ]

  | TkSemicolon -- ;
  | TkComma     -- ,
  | TkDot       -- .
  | TkColon     -- :
  | TkPercent   -- %

  | TokEOF
  deriving (Eq, Show)

tokPosToTuple :: Token -> (Int, Int, Int)
tokPosToTuple = tokPos
