module Normalise.Desugar(desugar) where

import Ast.Expression
import Ast.ExprTree
import Ast.Type
import Parser.Tokens
import Data.Tuple
import Data.Fixed
import Helpers.ErrorReport

desugar :: ExprTree ParserExpression -> ExprTree ParserExpression
desugar = foldConstants . desugarInitialisations

desugarInitialisations = fst . recursivelyStatefulTreeApplyTopDown desugarInitialisationsf False
foldConstants = treeApplyBottomUp (\a l -> foldConstf $ ExprTree a l)

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

--takes the lexer position for error reporting.
constJoin :: Constant -> Constant -> ExpressionBase -> LexerPosition -> (Constant,ExprType)
constJoin lc rc b pos = case lc of
    ConstantBool lb -> case rc of
        ConstantBool rb -> case b of
            ExpressionAnd -> (ConstantBool $ lb && rb,TypeBool)
            ExpressionOr -> (ConstantBool $ lb || rb,TypeBool)
            ExpressionXor -> (ConstantBool $ lb `xor` rb,TypeBool)
            _ -> invalidop
        ConstantString rs -> strop (bool2str lb) rs
        _ -> typeerror
    ConstantInt li -> case rc of
        ConstantInt ri
            | isArithOp b -> intop li ri
            | isEqOp b -> eqop li ri
            | isCompOp b -> compop li ri
            | otherwise -> invalidop
        ConstantFloat rf -> fop (fromIntegral li) rf
        ConstantString rs -> strop (show li) rs
        _ -> typeerror
    ConstantFloat lf -> case rc of
        ConstantInt ri -> fop lf $ fromIntegral ri
        ConstantFloat rf -> fop lf rf
        ConstantString rs -> strop (show lf) rs
        _ -> typeerror
    ConstantString ls -> case rc of
        ConstantBool rb -> strop ls $ bool2str rb
        ConstantInt ri -> strop ls $ show ri
        ConstantFloat rf -> strop ls $ show rf
        ConstantString rs -> strop ls rs
    where typeerror = error $ reportPos pos ++ "incopatible types : " ++ show lc ++ " and " ++ show rc
          invalidop = error $ reportPos pos ++ "invalid operation on : " ++ show lc ++ " and " ++ show rc ++ " : " ++ show b
          a `xor` b = (a || b) && not (a && b)
          bool2str a = if a then "true" else "false"
          fop l r = case () of
            _
                | isArithOp b -> floatop l r
                | isEqOp b -> eqop l r
                | isCompOp b -> compop l r
                | otherwise -> invalidop
          strop l r = (ConstantString $ case b of
              ExpressionArithPlus -> l ++ r
              _ -> invalidop,
            TypeString)
          eqop l r = (ConstantBool $ case b of
              ExpressionEq -> l == r
              ExpressionNeq ->  l /= r
              _ -> invalidop
              , TypeBool)
          compop l r = (ConstantBool $ case b of
              ExpressionLeq -> l <= r
              ExpressionLt -> l < r
              ExpressionGt -> l > r
              ExpressionGeq -> l >= r
              , TypeBool)
          intop li ri = (ConstantInt $ case b of
              ExpressionArithDiv -> li `div` ri
              ExpressionArithMod -> li `mod` ri
              _ -> numopcase li ri
              ,TypeInt)
          floatop lf rf = (ConstantFloat $ case b of
              ExpressionArithDiv -> lf / rf
              ExpressionArithMod -> lf `mod'` rf
              _ -> numopcase lf rf
              , TypeFloat)
          numopcase l r = case b of
              ExpressionArithPlus -> l + r
              ExpressionArithMinus -> l - r
              ExpressionArithMul -> l * r
              _ -> typeerror


foldConstf :: ExprTree ParserExpression -> ExprTree ParserExpression
foldConstf i@(ExprTree (t,b,p) [
    ExprTree (stl,ExpressionConstant lc,spl) [],
    ExprTree (rtl,ExpressionConstant rc,str) [] ])
    | isOperation b =
        let (c,t) = constJoin lc rc b p in
        ExprTree (t,ExpressionConstant c,p) []
    | otherwise  = i
foldConstf i@(ExprTree (t,b,p) [
    ExprTree (st,ExpressionConstant c,sp) [] ])
    | isOperation b =
        let const1 = ConstantInt 1
            (nc,t) = uncurry (\a b -> constJoin c a b p) $ case b of
                ExpressionInc -> (const1,ExpressionArithPlus)
                ExpressionDec -> (const1,ExpressionArithMinus)
                ExpressionNot -> (ConstantBool True,ExpressionXor)
                _ -> error $ reportPos p ++ show b ++ " not supported here."
        in ExprTree (t,ExpressionConstant nc,sp) []
    | otherwise = i
foldConstf a = a
