module Ast.ExprTree where

data ExprTree a = ExprTree a [ExprTree a]

treeApplyBottomUp :: ExprTree a -> (a -> [ExprTree b] -> ExprTree b) -> ExprTree b
ExprTree a l `treeApplyBottomUp` f = f a $ map (`treeApplyBottomUp` f) l

treeApplyTopDown :: ExprTree a -> (a -> [ExprTree a] -> (b,[ExprTree a])) -> ExprTree b
ExprTree a l `treeApplyTopDown` f =
    let (b,nl) = f a l in
    ExprTree b $ map (`treeApplyTopDown` f) nl

statefulTreeApplyTopDown :: ExprTree a -> s -> ( a -> [ExprTree a] -> s -> (b,[ExprTree a],s) ) -> (ExprTree b,s)
statefulTreeApplyTopDown (ExprTree e l) s f =
    let (b,l1,s1) = f e l s
        (l2,s2) = statefulMap (\a b -> statefulTreeApplyTopDown a b f) l1 s1
    in (ExprTree b l2,s2)

statefulMap :: (a -> s -> (b,s)) -> [a] -> s -> ([b],s)
statefulMap _ [] s = ([],s)
statefulMap f (x:xs) s =
    let (b,s1) = f x s
        (bl,s2) = statefulMap f xs s1
    in (b:bl,s2)

treeReturn :: a -> ExprTree a
treeReturn a = ExprTree a []
