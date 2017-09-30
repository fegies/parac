module Parser.ModuleParser( parseModule ) where

import Text.Parsec
import Data.Char
import Text.Parsec.Char
import Ast.Ast
import Control.Monad
import Data.List

data ParserState = ParserState { tabDepath :: Int, inComment :: Bool }

initialParserState = ParserState 0 False

parseModule :: String -> String -> Either ParseError Module
parseModule filename content =
    runParser moduleParser initialParserState filename content

moduleParser :: Parsec String ParserState Module
moduleParser = do
    whitespace
    (signature, exports) <- moduleDeclaration
    whitespace
    importlist <- importListParser
    input <- getInput
    eof
    return $ Module signature exports importlist []

moduleDeclaration :: Parsec String ParserState (String, Maybe [String])
moduleDeclaration = do
    whitespace
    string "module"
    many1 space
    signature <- qualifiedName
    whitespace
    exportlist <- optionMaybe . try $ bracedList
    statementEnd
    return (signature, exportlist)

importListParser :: Parsec String ParserState [(String, Maybe [String], String)]
importListParser = many $ do
    whitespace
    string "import"
    many1 space
    pack <- qualifiedName
    qualifiers <- optionMaybe . try $ many1 space >> bracedList
    alias <- (try $ many1 space >> string "as" >> many1 space >> uppercasedWord) <|> return pack
    statementEnd
    whitespace
    return (pack, qualifiers, alias)

qualifiedName :: Parsec String ParserState String
qualifiedName = do
    packages <- lowercasedWord `endBy` char '.'
    modulename <- uppercasedWord
    return $ intercalate "." $ packages ++ [modulename]

bracedList :: Parsec String ParserState [String]
bracedList = between (char '(') (char ')') $ (nonewlinewhite >> many1 letter >>= (\a -> nonewlinewhite >> return a)) `sepBy` (char ',')

statementEnd :: Parsec String ParserState ()
statementEnd = nonewlinewhite >> (endOfLine <|> char ';' <|> (lineComment >> return '\n')) >> return ()

uppercasedWord :: Parsec String ParserState String
uppercasedWord = do
    firstLetter <- upper
    rest <- many letter
    return $ firstLetter : rest
lowercasedWord :: Parsec String ParserState String
lowercasedWord = do
    firstLetter <- lower
    rest <- many letter
    return $ firstLetter : rest

nonewlinewhite :: Parsec String ParserState ()
nonewlinewhite = optional . many . satisfy $ \c -> c /= '\n' && isSpace c

lineComment :: Parsec String ParserState ()
lineComment = nonewlinewhite >> (try $ string "//" >> anyChar `manyTill` endOfLine) >> return ()

whitespace :: Parsec String ParserState ()
whitespace = skipMany $
    try (space `manyTill` endOfLine >> return ())
    <|> (try $ satisfy (\c -> isSpace c && c /= '\t' ) >> return ())
    <|> lineComment
    <|> (try $ string "/*" >> anyChar `manyTill` string "*/" >> return ())