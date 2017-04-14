module Tree where

data Tree a = Tree a [Tree a]

treeApply :: Tree a -> ( a -> [Tree b] -> Tree b) -> Tree b
Tree a l `treeApply` f = f a $ map (`treeApply` f) l

treeReturn :: a -> Tree a
treeReturn a = Tree a []
