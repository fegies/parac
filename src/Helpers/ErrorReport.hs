module Helpers.ErrorReport where

import Parser.Tokens

reportPos (_,l,c) = "At line: " ++ show  l ++ ", column " ++ show c ++ ": "
