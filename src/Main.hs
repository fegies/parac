
import System.Environment
import Parser
import Text.Show.Pretty

main = getArgs >>= parseArgs

usage = putStrLn "Usage: parac [path to main file] [path to output file]"


getOutputFunction filename = if filename == "-" then putStrLn else writeFile filename

parseArgs [mainfile, outputfile] = parse mainfile >>= (return . ppShow) >>= getOutputFunction outputfile
parseArgs _ = usage
