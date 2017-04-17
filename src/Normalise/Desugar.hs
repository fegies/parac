module Normalise.Desugar(desugar) where

import Ast.Expression
import Ast.ExprTree
import Ast.Type
import Parser.Tokens
import Data.Tuple
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

todominantType :: Constant -> Constant -> LexerPosition -> (Constant,Constant)
todominantType lc rc pos = case lc of
    ConstantBool lb -> case rc of
        ConstantBool _ -> (lc,rc)
        ConstantString _ -> (ConstantString $ show lb,rc)
        _ -> errorr
    ConstantInt li -> case rc of
        ConstantInt _ -> (lc,rc)
        ConstantFloat _ -> (ConstantFloat $ fromIntegral li,rc)
        ConstantString _ -> (ConstantString $ show li,rc)
        _ -> errorr
    ConstantFloat lf -> case rc of
        ConstantFloat _-> (lc,rc)
        ConstantString _ -> (ConstantString $ show lf,rc)
        _ -> swcall
    ConstantString _ -> case rc of
        ConstantString _ -> (lc,rc)
        _ -> swcall
    where errorr = error $ reportPos pos ++ "incopatible types : " ++ show lc ++ " and " ++ show rc
          swcall = swap $ todominantType rc lc pos

constJoin :: Constant -> Constant -> ExpressionBase -> LexerPosition -> (Constant,ExprType)
constJoin lc rc b pos = case lc of
    ConstantBool lb -> case rc of
        ConstantBool rb -> case b of
            ExpressionAnd -> (ConstantBool $ lb && rb,TypeBool)
            ExpressionOr -> (ConstantBool $ lb || rb,TypeBool)
            ExpressionXor -> (ConstantBool $ lb `xor` rb,TypeBool)
            _ -> invalidop
        ConstantString rs -> case b of
            ExpressionArithPlus -> (ConstantString $ show lb ++ rs,TypeString)
            _ -> invalidop
        _ -> typeerror
    ConstantInt li -> case rc of
        ConstantInt ri
            | isArithOp b -> intop li ri
    where typeerror = error $ reportPos pos ++ "incopatible types : " ++ show lc ++ " and " ++ show rc
          invalidop = error $ reportPos pos ++ "invalid operation on : " ++ show lc ++ " and " ++ show rc ++ " : " ++ show b
          a `xor` b = (a || b) && not (a && b)
          numop l r = case b of
              ExpressionArithPlus -> l + r
              ExpressionArithMinus -> l - r
              ExpressionArithMul -> l * r
              ExpressionArithDiv -> l `div` r
              ExpressionArithMod -> l `mod` r
              _ -> typeerror
          eqop l r = case b of
              ExpressionEq -> l == r
          intop li ri = (ConstantInt $ numop li ri,TypeInt)


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

isArithOp a = case a of
    ExpressionArithPlus -> True
    ExpressionArithMinus -> True
    ExpressionArithMul -> True
    ExpressionArithDiv -> True
    ExpressionArithMod -> True
    _ -> False

isSingleOp a = case a of
    ExpressionInc -> True
    ExpressionDec -> True
    ExpressionNot -> True
    _ -> False

isLogicOp a = case a of
    ExpressionAnd -> True
    ExpressionOr -> True
    _ -> False

isEqOp a = case a of
    ExpressionXor -> True
    ExpressionEq -> True
    ExpressionNeq -> True
    ExpressionLeq -> True
    ExpressionLt -> True
    ExpressionGt -> True
    ExpressionGeq -> True
    _ -> False

isOperation a = any (\f -> f a) [isArithOp,isSingleOp,isLogicOp,isEqOp]
