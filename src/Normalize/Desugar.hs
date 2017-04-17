module Normalize.Desugar(desugar) where

import Ast.Expression
import Ast.ExprTree
import Ast.Type
import Helpers.ErrorReport

desugar :: ExprTree ParserExpression -> ExprTree ParserExpression
desugar = desugarInitialisations

desugarInitialisations = fst . recursivelyStatefulTreeApplyTopDown desugarInitialisationsf False

desugarInitialisationsf :: ParserExpression -> [ExprTree ParserExpression] -> Bool
    -> (ParserExpression,[ExprTree ParserExpression],Bool)
desugarInitialisationsf p@(_,ExpressionBlock,_) l _ = (p,concatMap desugarInitialisationsff l,True)
desugarInitialisationsf (_,ExpressionVarDeclaration _,p) _ False =
    error $ reportPos p ++ "Variable Declarations only allowed in Block."
desugarInitialisationsf p l _ = (p,l,False)

desugarInitialisationsff :: ExprTree ParserExpression -> [ExprTree ParserExpression]
desugarInitialisationsff (ExprTree (_,ExpressionVarDeclaration d@(Declarator n t),p) [e]) =
    [
        ExprTree (TypeVoid,ExpressionVarDeclaration d,p) [],
        ExprTree (t,ExpressionAssign,p) [
            ExprTree (UnknownType,ExpressionLookup n,p) [],
            e
    ]]
desugarInitialisationsff a = [a]
