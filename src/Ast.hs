module Ast where

data Expression
    = EmptyExpression
    | ExpressionReturn Expression
    | ExpressionLoad String
    | ExpressionNew String
    | ExpressionLookup Identifier
    | ExpressionAssign Identifier Expression
    | ExpressionFunctionCall Expression [Expression]

    | ExpressionVarDeclaration String
    | ExpressionNamedFunctionDeclaration String [String] [Expression]
    | ExpressionClassDeclaration String [Expression]
    | ExpressionAnonFunctionDeclaration [String] [Expression]

    | ExpressionIf Expression [Expression] [Expression]
    | ExpressionWhile Expression [Expression]

    | ExpressionArithPlus Expression Expression
    | ExpressionArithMinus Expression Expression
    | ExpressionArithMul Expression Expression
    | ExpressionArithDiv Expression Expression
    | ExpressionArithMod Expression Expression
    | ExpressionInc Expression
    | ExpressionDec Expression
    | ExpressionNot Expression
    | ExpressionAnd Expression Expression
    | ExpressionOr Expression Expression

    | ExpressionEq Expression Expression
    | ExpressionNeq Expression Expression
    | ExpressionLeq Expression Expression
    | ExpressionLt Expression Expression
    | ExpressionGt Expression Expression
    | ExpressionGeq Expression Expression

    | ExpressionConstant Constant

    | SECheckedExpression Bool Expression
    deriving (Show)

data Identifier
    = IdentifierName String
    | IdentifierObjMember Identifier String
    | IdentifierArray Identifier Expression
    deriving (Show)

data Constant
    = ConstantInt Integer
    | ConstantString String
    deriving (Show)
