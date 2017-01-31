module Main where
import Lexer
import PseudocodeParser
import AstTransform
import ToBytecode
import System.Environment
import Hexdump
import qualified Data.ByteString.Lazy as BL
--main = getContents >>= print . parsePSC . lexer


main = getArgs >>= parseArgs

parseArgs [] = usage
parseArgs ("-h":_) = usage
parseArgs ("-d":xs) = interfiles xs
parseArgs x = prfiles x

prfiles [] = return ()
prfiles (x:xs) = do
    s <- readFile $ x
    let instr = transformToInstructions . normaliseAst . parsePSC . lexer $ s
    BL.putStr . toByecode $ instr
    prfiles xs

interfiles [] = return ()
interfiles (x:xs) = interpretFile x >> interfiles xs

usage = putStrLn "Usage: pcompile [-d] file[s]"


interpretFile f = do
    s <- readFile $ f
    let tokens = lexer s
    let ast = parsePSC tokens
    let normast = normaliseAst ast
    let instr = transformToInstructions normast
    let bytecode = toByecode instr

    putStrLn $ "--source--\n\n" ++ s
    putStrLn $ "\n--tokens--\n\n" ++ show tokens
    putStrLn $ "\n--ast--\n\n" ++ show ast
    putStrLn $ "\n--normalized ast--\n\n" ++ show normast
    putStrLn $ "\n--Instructions--\n\n" ++ show instr
    putStrLn "\n--Bytecode\n\n"
    putStrLn . prettyHex . BL.toStrict $ bytecode
