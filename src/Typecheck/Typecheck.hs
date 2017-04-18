module Typecheck.Typecheck(typecheck,Context) where

import qualified Data.Map.Strict as Map
import Ast.Type
import Ast.Expression
import Ast.ExprTree
import Parser.Tokens

type Context = Map.Map String ExprType

type ContextStack = [Context]

type ContextfulExpression = (ExprType,ExpressionBase,LexerPosition,Context)

typecheck = fst . statefulTreeApplyTopDownBottomUp typecheckDown typecheckUp []

typecheckDown :: ExprTree ParserExpression -> ContextStack -> (ContextfulExpression,[ExprTree ParserExpression],ContextStack)
typecheckDown a s = undefined

typecheckUp :: ExprTree ContextfulExpression -> ContextStack -> (ExprTree ContextfulExpression,ContextStack)
typecheckUp a s = undefined

unifyContexts :: Context -> Context -> Maybe Context
unifyContexts = undefined

mergeMapWithMaybe :: (a -> a -> Maybe a) -> Map.Map k a -> Map.Map k a -> Maybe (Map.Map k a)
mergeMapWithMaybe f lm rm = Map.traverseWithKey (const (>>=)) $
   Map.unionWith (\(Just l) (Just r) -> f l r) (Map.map return lm) (Map.map return rm)

unifyTypes :: ExprType -> ExprType -> Maybe ExprType
unifyTypes UnknownType b = Just b
unifyTypes a UnknownType = Just a
unifyTypes a b
    | a == b = Just a
    | otherwise = Nothing
