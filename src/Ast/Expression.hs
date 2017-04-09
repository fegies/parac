module Ast.Expression where

import qualified Data.Map as Map
import Tokens(LexerPosition)
import Ast.Type

type Expression = (ExprType, CompilerInfo, ExpressionBase,LexerPosition)

unknownLexPos = (-1,-1,-1)
emptyExpression = (TypeVoid, (TaintLevel 0, []), EmptyExpression, unknownLexPos)

defaultCompilerinfo = (UnknownPurity,[])

liftExpression :: ExpressionBase -> LexerPosition -> Expression
liftExpression a p = (UnknownType, (UnknownPurity,[]), a, p)

data ExpressionBase
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
    | ExpressionNamedFunctionDeclaration String [Declarator] ExprType Expression
    | ExpressionTypedefDeclaration String [Expression]
    | ExpressionAnonFunctionDeclaration [Declarator] ExprType Expression

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

    deriving (Show)

type CompilerInfo = (Taint,[Metadata])


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

identifierApply :: Identifier -> (Expression -> Expression) -> Identifier
identifierApply (IdentifierObjMember i s) f = IdentifierObjMember (identifierApply i f) s
identifierApply (IdentifierArray i e) f = IdentifierArray (identifierApply i f) (f e)
identifierApply a _ = a

astApply :: Expression -> (Expression -> Expression) -> Expression
astApply (a,b,e,p) f = (a,b,astApplyBase e f,p)

--applys the function to the subexpressions of an expression, but NOT to the expression itself
astApplyBase :: ExpressionBase -> (Expression -> Expression) -> ExpressionBase
astApplyBase (ExpressionReturn e) f = ExpressionReturn $ f e
astApplyBase (ExpressionAssign i e) f = ExpressionAssign i $ f e
astApplyBase (ExpressionFunctionCall e l) f = ExpressionFunctionCall (f e) (map f l)
astApplyBase (ExpressionBlock b) f = ExpressionBlock $ map f b
astApplyBase (ExpressionLookup i) f = ExpressionLookup $ identifierApply i f
astApplyBase (ExpressionVarDeclaration d e) f = ExpressionVarDeclaration d $ f e
astApplyBase (ExpressionNamedFunctionDeclaration s d1 d2 e) f = ExpressionNamedFunctionDeclaration s d1 d2 $ f e
astApplyBase (ExpressionAnonFunctionDeclaration d t e) f = ExpressionAnonFunctionDeclaration d t $ f e
astApplyBase (ExpressionIf e1 e2 e3) f = ExpressionIf (f e1) (f e2) (f e3)
astApplyBase (ExpressionWhile e1 e2) f = ExpressionWhile (f e1) (f e2)
astApplyBase (ExpressionArithPlus e1 e2) f = ExpressionArithPlus (f e1) (f e2)
astApplyBase (ExpressionArithMinus e1 e2) f = ExpressionArithMinus (f e1) (f e2)
astApplyBase (ExpressionArithMul e1 e2) f = ExpressionArithMul (f e1) (f e2)
astApplyBase (ExpressionArithDiv e1 e2) f = ExpressionArithDiv (f e1) (f e2)
astApplyBase (ExpressionArithMod e1 e2) f = ExpressionArithMod (f e1) (f e2)
astApplyBase (ExpressionInc e) f = ExpressionInc $ f e
astApplyBase (ExpressionDec e) f = ExpressionDec $ f e
astApplyBase (ExpressionNot e) f = ExpressionNot $ f e
astApplyBase (ExpressionAnd e1 e2) f = ExpressionAnd (f e1) (f e2)
astApplyBase (ExpressionOr e1 e2) f = ExpressionOr (f e1) (f e2)
astApplyBase (ExpressionEq e1 e2) f = ExpressionEq (f e1) (f e2)
astApplyBase (ExpressionNeq e1 e2) f = ExpressionNeq (f e1) (f e2)
astApplyBase (ExpressionLeq e1 e2) f = ExpressionLeq (f e1) (f e2)
astApplyBase (ExpressionLt e1 e2) f = ExpressionLt (f e1) (f e2)
astApplyBase (ExpressionGt e1 e2) f = ExpressionGt (f e1) (f e2)
astApplyBase (ExpressionGeq e1 e2) f = ExpressionGeq (f e1) (f e2)
astApplyBase e _ = e

astConstant :: ExprType -> Constant -> Expression
astConstant t a = (t, (TaintLevel 0,[]), ExpressionConstant a, unknownLexPos)

setType :: ExprType -> Expression -> Expression
setType nt (t,i,e,p) = (nt,i,e,p)
