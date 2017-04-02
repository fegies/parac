module Tokens where

type LexToken = (LexerPosition,Token)

type LexerPosition = (
    Int, --absolute offset
    Int, --line
    Int --column
    )

data Token
    = TokenIf -- Keywords
    | TokenElse
    | TokenWhile
    | TokenFunction
    | TokenTypedef
    | TokenVar
    | TokenReturn
    | TokenNew
    | TokenLoad
    | TokenSemicolon --Symbols
    | TokenColon
    | TokenComma
    | TokenRBOpen --round brace
    | TokenRBClose
    | TokenCBOpen --curly brace
    | TokenCBClose
    | TokenSBOpen --square brackets
    | TokenSBClose
    | TokenRightarrow
    | TokenAssign
    | TokenDot
    | TokenInt Integer
    | TokenCompEq
    | TokenCompNeq
    | TokenCompLeq
    | TokenCompLt
    | TokenCompGeq
    | TokenCompGt
    | TokenArithPlus
    | TokenArithMinus
    | TokenArithMul
    | TokenArithDiv
    | TokenArithMod
    | TokenArithInc
    | TokenArithDec
    | TokenLogicNot
    | TokenLogicAnd
    | TokenLogicOr
    | TokenStringLit String
    | TokenWord String
    | TokenTainted
    | TokenPure
    deriving (Show)
