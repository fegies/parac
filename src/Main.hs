module Main where
import Lexer
import Parser
import Normalise.Desugar
import System.Environment
import Ast.Dump
import Data.String
import Hexdump
import Control.Monad
import qualified Data.Map as Map
import System.Directory
import qualified Data.ByteString.Lazy as BL
--main = getContents >>= print . parsePSC . lexer

type CompilerFlags = Map.Map String String

defaultFlags :: CompilerFlags
defaultFlags = Map.fromList
    [
        ("dump", "")
    ]

main = getArgs >>= (\a -> if null a then usage >> return [] else return a) >>= flip parseArgs defaultFlags

parseArgs [] _ = return ()
parseArgs ("-h":_) _ = usage
parseArgs (x:xs) f = case stripPrefix "-D" x of
    Just arg -> case arg of
        "dump" -> case xs of
            [] -> putStrLn "No dump directory specified"
            (x:xs) -> createDirectoryIfMissing True x >> parseArgs xs (Map.insert "dump" x f)
        _ -> putStrLn $ "Invalid define name: " ++ arg
    Nothing ->  interpretFile x f >> parseArgs xs f


usage = putStrLn "Usage: pcompile [-d] file[s]"

interpretFile f flags = do
    let dumpdir = flags Map.! "dump"
    s <- readFile f
    let tokens = lexer s

    let ast = parse tokens
    unless (dumpdir == "") $
        writeFile (dumpdir ++ "/parsedAst.dump" ) $ dump ast
    let desugaredAst = desugar ast
    let normast = "normaliseAst desugaredAst"
    let instr = "transformToInstructions normast"
    let bytecode = "toByecode instr"

    putStrLn $ "--source--\n\n" ++ s
    putStrLn $ "\n--tokens--\n\n" ++ show tokens
    putStrLn $ "\n--ast--\n\n" ++ show ast
    putStrLn $ "\n--desugared ast--\n" ++ show desugaredAst
    putStrLn $ "\n--normalized ast--\n\n" ++ show normast
    putStrLn $ "\n--Instructions--\n\n" ++ show instr
    putStrLn "\n--Bytecode\n\n"
    putStrLn ". prettyHex . BL.toStrict $ bytecode"

stripPrefix :: Eq a => [a] -> [a] -> Maybe [a]
stripPrefix [] b = Just b
stripPrefix _ [] = Nothing
stripPrefix (x:xs) (y:ys) =
    if x == y then
        stripPrefix xs ys
    else
        Nothing
