module Ast where

data Expression
    = EmptyExpression
    | ExpressionReturn Expression
    --the list represents the packages, and the last entry is the file
    | ExpressionLoad [String] String
    | ExpressionNew String
    | ExpressionLookup Identifier
    | ExpressionAssign Identifier Expression
    | ExpressionFunctionCall Expression [Expression]

    | ExpressionBlock [Expression]

    --the expression is the value to initialize it to
    | ExpressionVarDeclaration Declarator Expression
    | ExpressionNamedFunctionDeclaration String [Declarator] Expression
    | ExpressionTypedefDeclaration String [Expression]
    | ExpressionAnonFunctionDeclaration [Declarator] Expression

    | ExpressionIf Expression Expression Expression
    | ExpressionWhile Expression Expression

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

    --the bool is true if the Expression is pure
    | SESetExpression Bool Expression
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

data Declarator
    = DeclaratorName String
    | DeclaratorTyped String String
    deriving (Show)

getExpVarDecDecl :: Expression -> Declarator
getExpVarDecDecl (ExpressionVarDeclaration a _) = a
getExpVarDecDecl _ = error "this is supposed to be used inside the parser."
