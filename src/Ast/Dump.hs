module Ast.Dump(dump) where

import Ast.Expression
import Ast.Type

dump :: Expression -> String
dump e = rmdoublenewline $ dumpf e 0

rmdoublenewline :: String -> String
rmdoublenewline [] = []
rmdoublenewline ('\n':'\n':xs) = rmdoublenewline $ '\n' : xs
rmdoublenewline (x:xs) = x : rmdoublenewline xs

dumpf :: Expression -> Int -> String
dumpf (t,i,b,p) tabsnum =
    let dtp1 = flip dumpf (tabsnum + 1)
        tabs = replicate tabsnum '\t'
        tabsp1 = tabs ++ "\t"
        sbl a = "[\n" ++ concatMap dtp1 a ++ tabs ++ "]"
        pbl a = "{\n" ++ concatMap ((tabsp1 ++) . dumpdecl) a ++ tabs ++ "}"
        dtp12 a b = dtp1 a ++ tabs ++ "With\n" ++ dtp1 b
    in
    tabs ++ show p ++ " " ++
    case b of
        EmptyExpression -> "EmptyExpression"
        ExpressionReturn e -> "Return\n" ++  dtp1 e
        ExpressionLoad s1 s2 -> "Load " ++ foldr (\a b -> a ++ "." ++ b) s2 s1
        ExpressionNew s -> "New " ++ s
        ExpressionLookup i -> "Lookup " ++ show i
        ExpressionAssign i e -> "Assign to " ++ show i ++ "\n" ++ dtp1 e
        ExpressionFunctionCall e a -> "Call \n" ++ dtp1 e ++ tabs ++ "with arguments " ++ sbl a
        ExpressionBlock e -> "Block" ++ sbl e
        ExpressionVarDeclaration d e -> "Declare " ++ dumpdecl d ++ tabs ++ " with value\n" ++ dtp1 e
        ExpressionNamedFunctionDeclaration n d t e -> "Function" ++ n ++ " with parameters " ++ pbl d ++ " with Exp\n" ++ dtp1 e
        ExpressionAnonFunctionDeclaration d t e -> "Lambda with parameters " ++ pbl d ++ " with Exp\n" ++ dtp1 e
        ExpressionTypedefDeclaration s d -> "Typedef " ++ s ++ sbl d
        ExpressionIf c t e -> "If\n" ++ dtp1 c ++ tabs ++ "then\n" ++ dtp1 t ++ tabs ++ "else" ++ dtp1 e
        ExpressionWhile c e -> "While\n" ++ dtp1 c ++ "Do\n" ++ dtp1 e
        ExpressionArithPlus a b -> "Add\n" ++ dtp12 a b
        ExpressionArithMinus a b -> "Subtract\n" ++ dtp12 a b
        ExpressionArithMul a b -> "Multiply\n" ++ dtp12 a b
        ExpressionArithDiv a b -> "Divide\n" ++ dtp12 a b
        ExpressionArithMod a b -> "Modulo\n" ++ dtp12 a b
        ExpressionInc a -> "Increment\n" ++ dtp1 a
        ExpressionDec a -> "Decrement\n" ++ dtp1 a
        ExpressionNot a -> "Not\n" ++ dtp1 a
        ExpressionAnd a b -> "And\n" ++ dtp12 a b
        ExpressionOr a b -> "Or\n" ++ dtp12 a b
        ExpressionEq a b -> "Equals\n" ++ dtp12 a b
        ExpressionNeq a b -> "NotEquals\n" ++ dtp12 a b
        ExpressionLeq a b -> "Leq\n" ++ dtp12 a b
        ExpressionLt a b -> "Lt\n" ++ dtp12 a b
        ExpressionGt a b -> "Gt\n" ++ dtp12 a b
        ExpressionGeq a b -> "Geq\n" ++ dtp12 a b
        ExpressionConstant a -> "Constant " ++ show a
    ++ "\n"

dumptype :: ExprType -> String
dumptype = show

dumpdecl :: Declarator -> String
dumpdecl a = show a ++ "\n"
