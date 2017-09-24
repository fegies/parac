
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
    case parseModule filename cont of
        Left error -> return $ Left error
        Right mainModule -> do
            let mainModuleName = moduleName mainModule
            let mainImports = map (takeleft . (\(_,a,_) -> a)) $ importList mainModule
            moduleMapEither <- resolveModules (Map.singleton mainModuleName mainModule) $ mainImports
            return $ case moduleMapEither of
                Left error -> Left error
                Right moduleMap -> Right $ Ast (normalizeModules moduleMap) mainModuleName
    where
        basedir = Path.directory . Path.fromText . pack $ filename
        resolveImportpath importpath =
            let s = splitOn(".") importpath
                packages = init s
                file = last s ++ ".para"
            in unpack . (\(Right a) -> a) . Path.toText . Path.concat $ basedir : (map (Path.fromText . pack) $ packages ++ [file])
        normalizeModules modulemap =
            let mapf key (Module name importlist body) = Module name (map modf importlist) body
                modf (a,Left modname,c) = (\b -> (a,b,c)) . Right . unjust $ Map.lookup modname modulemap
                modf a = a
            in Map.mapWithKey mapf modulemap
        resolveModules modulemap [] = return $ Right modulemap
        resolveModules modulemap (curmod:xs) = do
            let filename = resolveImportpath curmod
            cont <- readFile filename
            case parseModule  filename cont of
                Left error -> return $ Left error
                Right mod -> do
                    let modulename = moduleName mod
                        newmap = Map.insert modulename mod modulemap
                        importlist = filter (flip Map.notMember $ newmap) (map (\(a,_,_) -> a) $ importList mod) ++ xs
                    resolveModules newmap importlist

takeleft (Left a) = a
unjust (Just a) = a