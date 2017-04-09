module Ast.Dump(dump) where

import Ast.Expression


dump :: Expression -> String
dump = show
