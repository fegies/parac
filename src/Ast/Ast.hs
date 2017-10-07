module Ast.Ast where

import Text.Parsec(SourcePos)
import qualified Data.Map as Map
import Parser.ParserAst

--data Ast = Ast { modules :: Map.Map ModuleSignature Module, mainModule :: ModuleSignature } deriving(Show)
type Ast = Module

{-

type ModuleSignature = String
data Module = Module { moduleSignature :: ModuleSignature , exportList :: Maybe [String],  importList :: [(String, Maybe [String], String)], body :: [Statement] } deriving(Show)

data Expression = Expression ExprBody SourcePos ExprType
    deriving(Show)

data Statement
    = TypeDeclaration String (Either ExprType [(String, ExprType)])
    | ClassDeclaration String [String] 
    | FunctionDefinition
    | InstanceDefinition
    deriving(Show)

data ExprBody
    = ExprImport { modulePath :: String , qualifiedAs :: String }
    | ExprBinop { operator :: Binop, left :: Expression, right :: Expression }
    deriving(Show)


data ExprType
    = Void
    | Function { argumentTypes :: [ExprType], returnType :: ExprType}
    | ExprTypeStringForm String
    deriving(Show)

data Binop
    = Plus
    | Minus
    | Multiply
    | Divide
    | Modulo
    deriving(Show)

-}