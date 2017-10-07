module Parser.ParserAst where

data ModuleSignature = ModuleSignature { 
    packages :: [String],
    moduleName :: String
} deriving (Show)

data Module = Module {
    moduleSignature :: ModuleSignature,
    exportList :: Maybe [String],
    importList :: [ImportStatement],
    moduleBody :: [Statement]
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
type TypeAnnotation = String
type FunctionSignature = (FunctionName,[TypeAnnotation], TypeAnnotation)

data Statement
    = DataDeclaration {
        dataDeclarationName :: String,
        dataDeclarationConstraints :: Maybe [(ClassName, VarName)],
        dataDeclarationVariables :: Maybe [VarName],
        dataDeclarationFields :: [(FieldName,FieldType)]
    }
    | EnumDeclaration {
        emumDeclarationName :: String,
        enumDeclarationConstraints :: Maybe [(ClassName, VarName)],
        enumDeclarationVariables :: Maybe [VarName],
        enumDeclarationFields :: [(FieldName, FieldType)]
    }
    | ClassDeclaration {
        classDeclarationName :: String,
        classDeclarationConstraints :: Maybe [(ClassName,VarName)],
        classDeclarationVariables :: Maybe [VarName],
        classDeclarationFields :: [FunctionSignature]
    }
    deriving(Show)