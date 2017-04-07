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
    | ExpressionNamedFunctionDeclaration String [Declarator] TypeDeclaration Expression
    | ExpressionTypedefDeclaration String [Expression]
    | ExpressionAnonFunctionDeclaration [Declarator] TypeDeclaration Expression

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

type TypeDeclaration = String

data Identifier
    = IdentifierName String
    | IdentifierObjMember Identifier String
    | IdentifierArray Identifier Expression
    deriving (Show)

data Constant
    = ConstantInt Integer
    | ConstantString String
    | ConstantBool Bool
    deriving (Show)

data Declarator
    = DeclaratorName String
    | DeclaratorTyped String TypeDeclaration
    deriving (Show)

getExpVarDecDecl :: Expression -> Declarator
getExpVarDecDecl (ExpressionVarDeclaration a _) = a
getExpVarDecDecl _ = error "this is supposed to be used inside the parser."


--applys the function to the subexpressions of an expression, but NOT to the expression itself
astApply :: Expression -> (Expression -> Expression) -> Expression
astApply (ExpressionReturn e) f = ExpressionReturn $ f e
astApply (ExpressionAssign i e) f = ExpressionAssign i $ f e
astApply (ExpressionFunctionCall e l) f = ExpressionFunctionCall (f e) (map f l)
astApply (ExpressionBlock b) f = ExpressionBlock $ map f b
astApply (ExpressionVarDeclaration d e) f = ExpressionVarDeclaration d $ f e
astApply (ExpressionNamedFunctionDeclaration s d1 d2 e) f = ExpressionNamedFunctionDeclaration s d1 d2 $ f e
astApply (ExpressionAnonFunctionDeclaration d t e) f = ExpressionAnonFunctionDeclaration d t $ f e
astApply (ExpressionIf e1 e2 e3) f = ExpressionIf (f e1) (f e2) (f e3)
astApply (ExpressionWhile e1 e2) f = ExpressionWhile (f e1) (f e2)
astApply (ExpressionArithPlus e1 e2) f = ExpressionArithPlus (f e1) (f e2)
astApply (ExpressionArithMinus e1 e2) f = ExpressionArithMinus (f e1) (f e2)
astApply (ExpressionArithMul e1 e2) f = ExpressionArithMul (f e1) (f e2)
astApply (ExpressionArithDiv e1 e2) f = ExpressionArithDiv (f e1) (f e2)
astApply (ExpressionArithMod e1 e2) f = ExpressionArithMod (f e1) (f e2)
astApply (ExpressionInc e) f = ExpressionInc $ f e
astApply (ExpressionDec e) f = ExpressionDec $ f e
astApply (ExpressionNot e) f = ExpressionNot $ f e
astApply (ExpressionAnd e1 e2) f = ExpressionAnd (f e1) (f e2)
astApply (ExpressionOr e1 e2) f = ExpressionOr (f e1) (f e2)
astApply (ExpressionEq e1 e2) f = ExpressionEq (f e1) (f e2)
astApply (ExpressionNeq e1 e2) f = ExpressionNeq (f e1) (f e2)
astApply (ExpressionLeq e1 e2) f = ExpressionLeq (f e1) (f e2)
astApply (ExpressionLt e1 e2) f = ExpressionLt (f e1) (f e2)
astApply (ExpressionGt e1 e2) f = ExpressionGt (f e1) (f e2)
astApply (ExpressionGeq e1 e2) f = ExpressionGeq (f e1) (f e2)
astApply (SESetExpression p e) f = SESetExpression p $ f e
astApply e _ = e
