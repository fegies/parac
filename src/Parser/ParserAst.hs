module Parser.ParserAst where

import Text.Parsec(SourcePos)

data ModuleSignature = ModuleSignature { 
    packages :: [String],
    moduleName :: String
} deriving (Show)

data Module = Module {
    moduleSignature :: ModuleSignature,
    exportList :: Maybe [String],
    importList :: [ImportStatement],
    moduleBody :: [ModuleBodyStatement]
} deriving (Show)

data ImportStatement = ImportStatement {
    importmodulesig :: ModuleSignature,
    importnames :: Maybe [String],
    alias :: String
} deriving(Show)

type FieldName = String
type FieldType = String
type ClassName = String
type VarName = String
type FunctionName = String
type FunctionSignature = (FunctionName,[TypeAnnotation], TypeAnnotation)
data TypeAnnotation
    = TypeAnnotationLiteral {
        typeAnnotationLiteralType :: String,
        typeAnnotationLiteralVars :: Maybe [VarName]
    }
    | TypeAnnotationFunction {
        typeAnnotationFunctionParams :: [TypeAnnotation],
        typeAnnotationFunctionReturn :: TypeAnnotation
    }
    | TypeAnnotationMonomorph
    deriving(Show)

data ModuleBodyStatement
    = DataDeclaration {
        dataDeclarationPosition :: SourcePos,
        dataDeclarationName :: String,
        dataDeclarationConstraints :: Maybe [(ClassName, VarName)],
        dataDeclarationVariables :: Maybe [VarName],
        dataDeclarationFields :: [(FieldName,TypeAnnotation)]
    }
    | EnumDeclaration {
        enumDeclarationPosition :: SourcePos,
        emumDeclarationName :: String,
        enumDeclarationConstraints :: Maybe [(ClassName, VarName)],
        enumDeclarationVariables :: Maybe [VarName],
        enumDeclarationFields :: [(FieldName, TypeAnnotation)]
    }
    | ClassDeclaration {
        classDeclarationPos :: SourcePos,
        classDeclarationName :: String,
        classDeclarationConstraints :: Maybe [(ClassName,VarName)],
        classDeclarationVariables :: Maybe [VarName],
        classDeclarationFields :: [(FunctionSignature, Maybe ([(FieldName,TypeAnnotation)],[Statement]) )]
    }
    | ModuleFunctionDeclaration FunctionDeclaration
    | InstanceDeclaration {
        instanceDeclarationPos :: SourcePos,
        instanceDeclarationName :: String,
        instanceDeclarationArg :: TypeAnnotation,
        instanceDeclarationFields :: [FunctionDeclaration]
    }
    deriving(Show)

data FunctionDeclaration = FunctionDeclaration {
    functionDeclarationPos :: SourcePos,
    functionDeclarationName :: String,
    functionDeclarationArgs :: [(FieldName,TypeAnnotation)],
    functionDeclarationReturn :: TypeAnnotation,
    functionDeclarationBody :: [Statement]
} deriving(Show)

data Literal
    = LiteralString String
    | LiteralInt Integer
    | LiteralFloat Double
    | LiteralEmptyTuple
    deriving(Show)

data Expression
    = ExpressionLiteral SourcePos Literal
    | ExpressionStructConstruction SourcePos [(FieldName,Expression)]
    | ExpressionArrayConstruction SourcePos [Expression]
    | ExpressionMonop SourcePos Operation Expression
    | ExpressionBinop SourcePos Operation Expression Expression
    | ExpressionIdentifier SourcePos String
    | ExpressionFunctionCall SourcePos Expression [Expression] --expression body followed by arguments
    | ExpressionDotOperator SourcePos Expression String --structure followed by identifier
    | ExpressionConstructor SourcePos String
    | ExpressionSwitch {
        expressionSwitchPos :: SourcePos,
        expressionSwitchSwitchExpr :: Expression,
        expressionSwitchCases :: [SwitchCase]
    } --SourcePos Expression [(String,[VarName],Expression)]
    deriving(Show)

data SwitchCase = SwitchCase {
    switchCaseEnumConstructor :: String,
    switchCaseCaptureVars :: [VarName],
    switchCaseExpr :: Expression
} deriving(Show)

data Operation
    = ArithMul
    | ArithDiv
    | ArithMod
    | ArithPlus
    | ArithMinus
    | Negate
    | OpNot
    | OpAnd
    | OpOr
    | Increment
    | Decrement
    | ArrayAccess
    | CompareLt
    | CompareLeq
    | CompareGt
    | CompareGeq
    | CompareEq
    | CompareNeq
    | Assign
    deriving(Show)

data Statement
    = VariableDeclaration {
        variableDeclarationPos :: SourcePos,
        variableDeclarationName :: String,
        variableDeclarationType :: TypeAnnotation,
        variableDeclarationExpression :: Maybe Expression
    }
    | StatementExpression SourcePos Expression
    deriving(Show)
