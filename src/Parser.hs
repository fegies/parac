
module Parser(parse) where

import Parser.ModuleParser
import Text.Parsec(ParseError)
import qualified Data.Map.Strict as Map
import Data.List.Split
import Data.Text (pack,unpack)
import qualified Filesystem.Path.CurrentOS as Path
import Parser.ParserAst

type Ast = Module


parse :: String -> IO (Either ParseError Ast)
parse filename = do 
    cont <- readFile filename
    return $ parseModule filename cont
takeleft (Left a) = a
unjust (Just a) = a