module Typecheck.Typecheck(typecheck,Context) where

import qualified Data.Map.Strict as Map
import Ast.Type
import Ast.Expression
import Ast.ExprTree
import Parser.Tokens
import Helpers.ErrorReport

type Context = Map.Map String ExprType

type ContextStack = [Context]

type ContextfulExpression = (ExprType,ExpressionBase,LexerPosition,Context)

typecheck = fst . statefulTreeApplyTopDownBottomUp typecheckDown typecheckUp [Map.empty]

typecheckDown :: ExprTree ParserExpression -> ContextStack -> (ContextfulExpression,[ExprTree ParserExpression],ContextStack)
typecheckDown (ExprTree (t,b,p) l) (sh:st) =
    case b of
        ExpressionBlock -> ac
        ExpressionAnonFunctionDeclaration _ _ -> ac
        ExpressionVarDeclaration (Declarator n t) -> let nc = Map.insert n t sh
            in ((t,b,p,nc),l,nc:st)
        _ -> ((t,b,p,sh),l,sh:st)
        where ac = ((t,b,p,sh),l,Map.empty:sh:st)

typecheckUp :: ExprTree ContextfulExpression -> ContextStack -> (ExprTree ContextfulExpression,ContextStack)
typecheckUp (ExprTree (t,b,p,c) l) (st:sr) = case b of
    ExpressionBlock -> (ExprTree (getExprType $ last l,b,p,unifiedContext) l,sr)
    ExpressionAnonFunctionDeclaration _ _ -> ubl
    _ -> (ExprTree (t,b,p,unifiedContext) l,unifiedContext:sr)
    where unifiedContext = unifyContexts c st p
          ubl = (ExprTree (t,b,p,unifiedContext) l,sr)

unifyContexts :: Context -> Context -> LexerPosition -> Context
unifyContexts l r p = Map.unionWith (unifyTypes p) l r



unifyTypes :: LexerPosition -> ExprType -> ExprType -> ExprType
unifyTypes _ UnknownType b = b
unifyTypes _ a UnknownType = a
unifyTypes  p a b
    | a == b = a
    | otherwise = error $ reportPos p ++ "Unification error. Found " ++ show a ++ ", expected " ++ show b

getExprType (ExprTree (t,_,_,_) _) = t

lookupinStack :: String -> ContextStack -> Maybe (Int,ExprType)
lookupinStack =
    let slook d k s =
            case s of
                [] -> Nothing
                (x:xs) -> case Map.lookup k x of
                    Just e -> Just (d,e)
                    Nothing -> slook (d+1) k xs
    in slook 0
