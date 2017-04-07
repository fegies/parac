module Desugar(desugar) where

import Ast


desugar :: Expression -> Expression
desugar(a,b,ExpressionInc e)
    = (a,b,
        case e of
            l @(_,_,ExpressionLookup i) -> ExpressionAssign i (liftAst $ ExpressionArithPlus l (constantAstInt 1))
            _ -> error "only Identifier allowed in increment"
    )
desugar(a,b,ExpressionDec e)
    = (a,b,
        case e of
            l @(_,_,ExpressionLookup i) -> ExpressionAssign i (liftAst $ ExpressionArithMinus l (constantAstInt 1))
            _ -> error "only Identifier allowed in decrement"
    )
desugar a = astApply a desugar
