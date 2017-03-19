module Tokens where

type LexToken = (LexerPosition,Token)

type LexerPosition = (
    Int, --absolute offset
    Int, --line
    Int --column
    )

data Token
    = TokenIf -- Keywords
    | TokenThen
    | TokenElse
    | TokenFi
    | TokenWhile
    | TokenDo
    | TokenOd
    | TokenRepeat
    | TokenUntil
    | TokenFor
    | TokenTo
    | TokenDownto
    | TokenFunction
    | TokenClass
    | TokenReturn
    | TokenNew
    | TokenSemicolon --Symbols
    | TokenComma
    | TokenRBOpen --round brace
    | TokenRBClose
    | TokenCBOpen --curly brace
    | TokenCBClose
    | TokenSBOpen --square brackets
    | TokenSBClose
    | TokenLeftarrow -- <-
    | TokenQuote -- "
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
    | TokenInstr Integer
    | TokenDollar
    deriving (Show)
