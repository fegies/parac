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

pfs =parsePSC . lexer $ t

s = "\
  \a <- \"hi\";\n\
  \while 1==1; do\n\
  \{\n\
  \    print(a);\n\
  \    sleep(1);\n\
  \}\n\
  \od\n"

b = "print(a); sleep(1);"

c = "\"hi\" ;"

t = "if 1==1 ; then {a <- (b == (c(d,e)) );}  fi"

e = "i <- 0; while i <= 5; do {print(i); a[i]<- b++; i <- i + 1; } od"

f = "repeat { i++; a[i] <- 4; } until i == 40;"

g = "function f (a,b,c) { print(a); print(b); } print( f (a) );"

h = "for i <- 0 to a; do print(i); od"

j = "for i <- 0 downto b; do { sleep(1); } od"
i = normaliseAst . parsePSC . lexer
