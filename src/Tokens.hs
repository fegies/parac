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
    | TokenTrue
    | TokenFalse
    | TokenWhile
    | TokenFunction
    | TokenTypedef
    | TokenVar
    | TokenReturn
    | TokenNew
    | TokenLoad
    | TokenBackslash
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
    | TokenFloat Double
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
