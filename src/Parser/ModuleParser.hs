module Parser.ModuleParser( parseModule ) where

import Text.Parsec
import Data.Char
import Text.Parsec.Char
import Ast.Ast
import Control.Monad
import Data.List
import Parser.ParaDef
import qualified Text.Parsec.Token as P
import Text.Parsec.Expr
import Data.List.Split(splitOn)
import Parser.ParserAst
import Control.Applicative(liftA,(<**>))

type ParserState = ()

initialParserState = ()

parseModule :: String -> String -> Either ParseError Module
parseModule filename content =
    runParser moduleParser initialParserState filename content

-- dummyParser = fail "not implemented yet"

lowerWord = (:) <$> lower <*> many letter
upperWord = (:) <$> upper <*> many letter
word = many letter

moduleIdentifierToModulename :: String -> String
moduleIdentifierToModulename = last . splitOn "."

moduleParser = do
    whiteSpace
    reserved "module"
    modsig <- moduleSignatureParser
    exportlist <- optionMaybe . try . parens . commaSep1 $ identifier
    semi
    imports <- many $ do
        reserved "import"
        modsig <- moduleSignatureParser
        qualifiers <- optionMaybe . try . parens . commaSep1 $ identifier
        alias <- (symbol "as" >> lexeme upperWord) <|> (pure $ moduleName modsig)
        semi
        return $ ImportStatement modsig qualifiers alias
    body <- many statementParser
    eof
    return $ Module modsig exportlist imports body

moduleSignatureParser = lexeme $ ModuleSignature <$> (lowerWord `endBy` char '.') <*> upperWord

statementParser = typeDeclarationParser <|> enumDeclarationParser <|> classDeclarationParser

typeVarsParser = (try . parens $ 
    (,) <$> (optionMaybe . try  $ ((many $ (,) <$> lexeme upperWord <*> lexeme lowerWord))
    <* reservedOp "=>")
    <*> (emptytoNothing <$> many (lexeme lowerWord))
    ) <|> pure (Nothing,Nothing)


dataDeclarationBodyParser constructor = do
    name <- lexeme upperWord
    (constraints, vars) <- typeVarsParser
    fields <- fieldsParser
    return $ constructor name constraints vars fields
    where
        fieldsParser = braces . commaSep1 $ (,) <$> identifier <* colon <*> lexeme word
typeDeclarationParser = reserved "data" >> dataDeclarationBodyParser DataDeclaration
enumDeclarationParser = reserved "enum" >> dataDeclarationBodyParser EnumDeclaration

classDeclarationParser = do
    reserved "class"
    name <- lexeme upperWord
    (constraints, vars) <- typeVarsParser
    fields <- braces . commaSep1 $ stripArgNames <$> functionSignatureParser
    return $ ClassDeclaration name constraints vars fields
    where stripArgNames (n,l,r) = (n,map snd l,r)

functionSignatureParser = do
    name <- lexeme lowerWord
    args <- parens . commaSep $ (,) <$> identifier <* colon <*> lexeme typeAnnotation
    colon
    rettype <- lexeme typeAnnotation
    return (name,args,rettype)

typeAnnotation = (try . lexeme $ upperWord) <|> lexeme lowerWord


emptytoNothing :: [a] -> Maybe [a]
emptytoNothing [] = Nothing
emptytoNothing a = Just a

-- token parser definitions
lexer = P.makeTokenParser paraDef
lexeme = P.lexeme lexer
reserved = P.reserved lexer
whiteSpace = P.whiteSpace lexer
parens = P.parens lexer
identifier = P.identifier lexer
commaSep1 = P.commaSep1 lexer
commaSep = P.commaSep lexer
semi = P.semi lexer
symbol = P.symbol lexer
braces = P.braces lexer
reservedOp = P.reservedOp lexer
colon = P.colon lexer