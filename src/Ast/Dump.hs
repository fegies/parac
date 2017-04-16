module Ast.Dump(dump) where

import Ast.Expression
import Ast.Type
import Ast.ExprTree

dump :: Show a => ExprTree a -> String
dump = delmulti '\n' . flip dump' 0

dump' :: Show a => ExprTree a -> Int -> String
dump' (ExprTree a l) tabs =
    replicate tabs '\t' ++ show a ++ '\n' : unlines (map (flip dump' $ tabs + 1) l)

--collapses multiple repeating instances of a into one
delmulti :: Eq a => a -> [a] -> [a]
delmulti _ [] = []
delmulti a (b:c:xs)
    | a == b && b == c = delmulti a (b:xs)
delmulti a (x:xs) = x : delmulti a xs
