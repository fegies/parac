module Main where
import Lexer
import Parser
import Desugar
import System.Environment
import Hexdump
import qualified Data.ByteString.Lazy as BL
--main = getContents >>= print . parsePSC . lexer


main = getArgs >>= parseArgs

parseArgs [] = usage
parseArgs ("-h":_) = usage
parseArgs ("-d":xs) = interfiles xs
parseArgs x = interfiles x

interfiles = foldr ((>>) . interpretFile) (return ())

usage = putStrLn "Usage: pcompile [-d] file[s]"


interpretFile f = do
    s <- readFile f
    let tokens = lexer s
    let ast = parse tokens
    let desugaredAst = desugar ast
    let normast = "normaliseAst ast"
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
