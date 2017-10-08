module Parser.ParserAst where

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
        dataDeclarationName :: String,
        dataDeclarationConstraints :: Maybe [(ClassName, VarName)],
        dataDeclarationVariables :: Maybe [VarName],
        dataDeclarationFields :: [(FieldName,TypeAnnotation)]
    }
    | EnumDeclaration {
        emumDeclarationName :: String,
        enumDeclarationConstraints :: Maybe [(ClassName, VarName)],
        enumDeclarationVariables :: Maybe [VarName],
        enumDeclarationFields :: [(FieldName, TypeAnnotation)]
    }
    | ClassDeclaration {
        classDeclarationName :: String,
        classDeclarationConstraints :: Maybe [(ClassName,VarName)],
        classDeclarationVariables :: Maybe [VarName],
        classDeclarationFields :: [(FunctionSignature, Maybe ([(FieldName,TypeAnnotation)],[Statement]) )]
    }
    | FunctionDeclaration {
        functionDeclarationName :: String,
        functionDeclarationArgs :: [(FieldName,TypeAnnotation)],
        functionDeclarationReturn :: TypeAnnotation,
        functionDeclarationBody :: [Statement]
    }
    deriving(Show)

data Literal
    = LiteralString String
    | LiteralInt Integer
    | LiteralFloat Double
    | LiteralEmptyTuple
    deriving(Show)

data Expression
    = ExpressionLiteral Literal
    | ExpressionStructConstruction [(FieldName,Expression)]
    | ExpressionArrayConstruction [Expression]
    | ExpressionMonop Operation Expression
    | ExpressionBinop Operation Expression Expression
    | ExpressionIdentifier String
    | ExpressionFunctionCall Expression [Expression] --expression body followed by arguments
    | ExpressionDotOperator Expression String --structure followed by identifier
    deriving(Show)

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
        variableDeclarationName :: String,
        variableDeclarationType :: TypeAnnotation,
        variableDeclarationExpression :: Maybe Expression
    }
    | StatementExpression Expression
    deriving(Show)
