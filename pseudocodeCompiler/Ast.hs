module Ast where

type Program = Block 

type Block = [Statement]

data Statement
    = StatementIf Expression Block Block
    | StatementWhile Expression Block
    | StatementRepeat Block Expression
    | StatementFunctionDeclaration String [String] Block -- name, arguments
    | StatementReturn Expression
    | StatementExpression Expression
    deriving (Show)

data Constant
    = ConstantInt Integer
    | ConstantString String 
    deriving (Show)

type FunctionArguments = [Expression]

data Expression
    = ExpressionVar String
    | ExpressionConstant Constant
    | ExpressionFunctionCall Expression FunctionArguments --funciton name, Arguments
    | ExpressionArrayAccess Expression Expression --array, position
    | ExpressionAssign Expression Expression --to ,from

    | ExpressionCompareEq Expression Expression
    | ExpressionCompareNeq Expression Expression
    | ExpressionCompareLt Expression Expression
    | ExpressionCompareGt Expression Expression
    | ExpressionCompareGeq Expression Expression
    | ExpressionCompareLeq Expression Expression

    | ExpressionArithPlus Expression Expression
    | ExpressionArithMinus Expression Expression
    | ExpressionArithMul Expression Expression
    | ExpressionArithDiv Expression Expression
    | ExpressionArithMod Expression Expression
    | ExpressionArithInc Expression
    | ExpressionArithDec Expression

    | ExpressionLogicNot Expression
    | ExpressionLogicAnd Expression Expression
    | ExpressionLogicOr Expression Expression

    deriving (Show)
