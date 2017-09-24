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
    modulename <- moduleDeclaration
    whitespace
    importlist <- importListParser
    input <- getInput
    pos <- getPosition
    return $ Module modulename importlist []

importListParser :: Parsec String ParserState [(String, Either String Module ,String)]
importListParser = many $ do
    string "import"
    nonewlineWhitespace
    importname <- qualifiedName
    qualifier <- optionMaybe $ do
        nonewlineWhitespace
        string "as"
        nonewlineWhitespace
        uppercasedWord
    optional nonewlineWhitespace
    optionalSemicolon
    return $ case qualifier of
        Just qual -> (importname, Left importname, qual)
        Nothing -> (importname, Left importname, "")

qualifiedName :: Parsec String ParserState String
qualifiedName = do
    optional nonewlineWhitespace
    packages <- lowercasedWord `endBy` char '.'
    modulename <- uppercasedWord
    return $ intercalate "." $ packages ++ [modulename]

moduleDeclaration :: Parsec String ParserState String
moduleDeclaration = do
    whitespace
    string "module"
    (try $ optional nonewlineWhitespace >> optionalSemicolon >> return "") <|> qualifiedName

optionalSemicolon :: Parsec String ParserState ()
optionalSemicolon = (endOfLine <|> char ';') >> return ()

nonewlineWhitespace = many1 $ satisfy (\c -> isSpace c && c /= '\n')

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

whitespace :: Parsec String ParserState ()
whitespace = do
    skipMany $ try $ space `manyTill` endOfLine
    skipMany $ satisfy (\c -> isSpace c && c /= '\t' )
    skipMany $ string "//" >> anyChar `manyTill` endOfLine
    skipMany $ string "/*" >> anyChar `manyTill` string "*/"