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
typecheckDown (ExprTree (t,b,p) l) (sh:st) = ((t,b,p,newContext),l,newStack)
    where newStack = if isSubcontextExp b then newSubContext : newContext : st else newContext : st
          newContext = case b of
              ExpressionVarDeclaration (Declarator n t) -> Map.insert n t sh
              ExpressionNamedFunctionDeclaration n _ t -> Map.insert n t sh
              _ -> sh
          newSubContext = case b of
              ExpressionNamedFunctionDeclaration _ d _ -> Map.fromList $ map (\(Declarator a b) -> (a,b)) d
              _ -> Map.empty

typecheckUp :: ExprTree ContextfulExpression -> ContextStack -> (ExprTree ContextfulExpression,ContextStack)
typecheckUp (ExprTree (t,b,p,c) l) (sh:st) = (ExprTree (newType,newBase,p,newContext) l,newStack)
    where
    newType = t
    newBase = b
    newContext = unifyContexts c sh p
    newStack = if isSubcontextExp b then st else newContext : st


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
