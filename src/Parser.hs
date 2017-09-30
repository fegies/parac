
module Parser(parse) where

import Parser.ModuleParser
import Text.Parsec(ParseError)
import Ast.Ast
import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.Text (pack,unpack)
import qualified Filesystem.Path.CurrentOS as Path


parse :: String -> IO (Either ParseError Ast)
parse filename = do 
    cont <- readFile filename
    return $ case parseModule filename cont of
        Left err -> Left err
        Right mod -> let modname = moduleSignature mod
                     in Right $ Ast (Map.singleton modname mod) modname

takeleft (Left a) = a
unjust (Just a) = a