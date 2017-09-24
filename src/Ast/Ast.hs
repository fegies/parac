
module Ast.Ast where

import Text.Parsec(SourcePos)
import qualified Data.Map as Map

data Ast = Ast { modules :: Map.Map String Module, mainModule :: String } deriving(Show)

data Module = Module { moduleName :: String, importList :: [(String, Either String Module, String)], body :: [Expression] } deriving(Show)

data Expression = Expression ExprBody SourcePos ExprType
    deriving(Show)


data ExprBody
    = ExprImport { modulePath :: String , qualifiedAs :: String }
    | ExprBinop { operator :: Binop, left :: Expression, right :: Expression }
    deriving(Show)


data ExprType
    = Void
    | Function { argumentTypes :: [ExprType], returnType :: ExprType}
    deriving(Show)

data Binop
    = Plus
    | Minus
    | Multiply
    | Divide
    | Modulo
    deriving(Show)
