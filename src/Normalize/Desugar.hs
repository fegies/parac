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
desugarInitialisationsf e@(_,ExpressionBlock,_) l _ = (e,concatMap desugarInitialisationsff l,True)
desugarInitialisationsf e@(_,ExpressionTypedefDeclaration _,_) l _ = (e,l,True)
desugarInitialisationsf e l False = case e of
    (_,ExpressionVarDeclaration _,p) -> error $ reportPos p ++ "Variable Delarations disallowed here"
    (_,ExpressionNamedFunctionDeclaration {},p) -> error $ reportPos p ++ "Function Declarations disallowed here"
    _ -> (e,l,False)
desugarInitialisationsf e l _ = (e,l,False)

desugarInitialisationsff :: ExprTree ParserExpression -> [ExprTree ParserExpression]
desugarInitialisationsff (ExprTree (_,ExpressionVarDeclaration d@(Declarator n t),p) [e]) =
    [
        ExprTree (TypeVoid,ExpressionVarDeclaration d,p) [],
        ExprTree (t,ExpressionAssign,p) [
            ExprTree (UnknownType,ExpressionLookup n,p) [],
            e
    ]]
desugarInitialisationsff a = [a]
