module Ast.ExprTree where

import Debug.Trace

data ExprTree a = ExprTree a [ExprTree a]


treeApplyBottomUp :: (a -> [ExprTree b] -> ExprTree b) -> ExprTree a -> ExprTree b
treeApplyBottomUp f (ExprTree a l) = f a $ map (treeApplyBottomUp f) l

treeApplyTopDown :: (a -> [ExprTree a] -> (b,[ExprTree a])) -> ExprTree a -> ExprTree b
treeApplyTopDown f (ExprTree a l) =
    let (b,nl) = f a l in
    ExprTree b $ map (treeApplyTopDown f) nl

statefulTreeApplyTopDown :: ( a -> [ExprTree a] -> s -> (b,[ExprTree a],s) )
    -> s -> ExprTree a -> (ExprTree b,s)
statefulTreeApplyTopDown f s (ExprTree e l) =
    let (b,l1,s1) = f e l s
        (l2,s2) = statefulMap (statefulTreeApplyTopDown f) l1 s1
    in (ExprTree b l2,s2)

--does not update state in mapping on the subfunctions
recursivelyStatefulTreeApplyTopDown :: ( a -> [ExprTree a] -> s -> (b,[ExprTree a],s))
    -> s -> ExprTree a -> (ExprTree b,s)
recursivelyStatefulTreeApplyTopDown f s (ExprTree e l) =
    let (b,l1,s1) = f e l s
        l2 = map (fst . recursivelyStatefulTreeApplyTopDown f s1) l1
    in (ExprTree b l2,s1)

statefulMap :: (s -> a -> (b,s)) -> [a] -> s -> ([b],s)
statefulMap _ [] s = ([],s)
statefulMap f (x:xs) s =
    let (b,s1) = f s x
        (bl,s2) = statefulMap f xs s1
    in (b:bl,s2)

statefulTreeApplyBottomUp :: (a -> [ExprTree b] -> s -> (ExprTree b,s)) -> s -> ExprTree a -> (ExprTree b,s)
statefulTreeApplyBottomUp f s (ExprTree a l) =
    let (nl,s1) = statefulMap (statefulTreeApplyBottomUp f) l s
    in f a nl s1

treeReturn :: a -> ExprTree a
treeReturn a = ExprTree a []
