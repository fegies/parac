module Ast.Type where

import qualified Data.Map as Map

data ExprType
    = UnknownType
    | TypeVoid
    --parameters and return type, the return type is the last entry
    | TypeFunction [ExprType]
    --typedef entries
    | TypeTypedef (Map.Map String ExprType)
    | TypeString
    | TypeInt
    | TypeBool
    | TypeFloat
    --the type of the array elements
    | TypeArray ExprType
    | TypeUnresolved String
    deriving (Show)
