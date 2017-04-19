module Ast.ExprTree where

import Debug.Trace
import Control.Applicative
import Control.Monad

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

--first applys the first function on the way down, then the second one on the way up
statefulTreeApplyTopDownBottomUp :: (ExprTree a -> s -> (b,[ExprTree a],s)) -> (ExprTree b -> s -> (ExprTree b,s)) -> s -> ExprTree a -> (ExprTree b,s)
statefulTreeApplyTopDownBottomUp fd fu s1 a =
    let (b,l1,s2) = fd a s1
        (l2,s3) = statefulMap (statefulTreeApplyTopDownBottomUp fd fu) l1 s2
    in fu (ExprTree b l2) s3

treeReturn :: a -> ExprTree a
treeReturn a = ExprTree a []

instance Functor ExprTree where
    fmap f (ExprTree p l) = ExprTree (f p) (map (fmap f) l)

instance Foldable ExprTree where
    foldr f b (ExprTree a l) = f a $ case l of
        [] -> b
        _ -> foldr (flip $ foldr f) b l
