module Normalise.Desugar(desugar) where

import Ast.Expression


desugar :: Expression -> Expression
desugar a = astApply a desugar
