module Ast.Expression where

import qualified Data.Map as Map
import Tokens(LexerPosition)
import Ast.Type

type ParserExpression = (ExprType, ExpressionBase,LexerPosition)


data ExpressionBase
    = EmptyExpression
    | ExpressionReturn
    --the list represents the packages, and the last entry is the file
    | ExpressionLoad [String] String
    | ExpressionNew String
    | ExpressionLookup Identifier
    | ExpressionAssign Identifier
    | ExpressionFunctionCall

    | ExpressionBlock

    --the expression is the value to initialize it to
    | ExpressionVarDeclaration Declarator
    | ExpressionNamedFunctionDeclaration String [Declarator] ExprType
    | ExpressionTypedefDeclaration String
    | ExpressionAnonFunctionDeclaration [Declarator] ExprType

    | ExpressionIf
    | ExpressionWhile

    | ExpressionArithPlus
    | ExpressionArithMinus
    | ExpressionArithMul
    | ExpressionArithDiv
    | ExpressionArithMod
    | ExpressionInc
    | ExpressionDec
    | ExpressionNot
    | ExpressionAnd
    | ExpressionPure
    | ExpressionTainted
    | ExpressionOr

    | ExpressionEq
    | ExpressionNeq
    | ExpressionLeq
    | ExpressionLt
    | ExpressionGt
    | ExpressionGeq

    | ExpressionConstant Constant
    deriving (Show)

type Metadata = (String,String)

data Taint
    = UnknownPurity
    | InfiniteTaint
    | TaintLevel Integer
    deriving(Show)

data Identifier
    = IdentifierName String
    | IdentifierObjMember Identifier String
    | IdentifierArray Identifier Expression
    deriving (Show)

data Constant
    = ConstantInt Integer
    | ConstantString String
    | ConstantBool Bool
    | ConstantFloat Double
    deriving (Show)

data Declarator
    = Declarator String ExprType
    deriving (Show)
