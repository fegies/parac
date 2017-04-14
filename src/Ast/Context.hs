module Ast.Context where

import qualified Data.Map as Map
import Ast.Expression
import Ast.Type
import Control.Monad

                        --variables             --child contexts --parent contexts
data Context = Context (Map.Map String Variable) [Context] [Expression]

type Variable = (String, ExprType, Integer)

unifyContexts :: Context -> Context -> Maybe Context
unifyContexts a b = Nothing
