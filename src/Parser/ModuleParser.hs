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
import Control.Applicative(liftA, liftA2)

type ParserState = ()

initialParserState = ()

parseModule :: String -> String -> Either ParseError Module
parseModule filename content =
    runParser moduleParser initialParserState filename content

-- dummyParser = fail "not implemented yet"

lowerWord = (:) <$> lower <*> many letter
upperWord = (:) <$> upper <*> many letter

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
        alias <- (symbol "as" >> lexeme upperWord) <|> pure  (moduleName modsig)
        semi
        return $ ImportStatement modsig qualifiers alias
    body <- many moduleBodyStatementParser
    eof
    return $ Module modsig exportlist imports body

moduleSignatureParser = lexeme $ ModuleSignature <$> (lowerWord `endBy` char '.') <*> upperWord

moduleBodyStatementParser = typeDeclarationParser <|> enumDeclarationParser <|> classDeclarationParser <|> functionDeclarationParser

typeVarsParser = (try . parens $ 
    (,) <$> (optionMaybe . try  $ many ((,) <$> lexeme upperWord <*> lexeme lowerWord)
    <* reservedOp "=>")
    <*> (emptytoNothing <$> many (lexeme lowerWord))
    ) <|> pure (Nothing,Nothing)
    where
        emptytoNothing [] = Nothing
        emptytoNothing a = Just a

functionDeclarationParser = do
    (name, args, rettype) <- functionSignatureParser
    body <- braces . many1 $ statementParser
    return $ FunctionDeclaration name args rettype body

dataDeclarationBodyParser constructor = do
    name <- lexeme upperWord
    (constraints, vars) <- typeVarsParser
    fields <- fieldsParser
    return $ constructor name constraints vars fields
    where
        fieldsParser = braces . commaSep1 $ (,) <$> identifier <*> typeAnnotationParser
typeDeclarationParser = reserved "data" >> dataDeclarationBodyParser DataDeclaration
enumDeclarationParser = reserved "enum" >> dataDeclarationBodyParser EnumDeclaration

classDeclarationParser = do
    reserved "class"
    name <- lexeme upperWord
    (constraints, vars) <- typeVarsParser
    fields <- braces . commaSep1 $ do
        signature <- functionSignatureParser
        definition <- optionMaybe . braces . many1 $ statementParser
        return (stripArgNames signature, fmap ((,) $ getFullArgs signature) definition)
    return $ ClassDeclaration name constraints vars fields
    where 
        stripArgNames (n,l,r) = (n,map snd l,r)
        getFullArgs (_,l,_) = l

functionSignatureParser = do
    name <- lexeme lowerWord
    args <- parens . commaSep $ (,) <$> identifier <*> typeAnnotationParser
    rettype <- typeAnnotationParser
    return (name,args,rettype)

typeAnnotationParser = (colon >> lexeme typeAnnotationParser') <|> pure TypeAnnotationMonomorph
    where
    typeAnnotationParser' = (TypeAnnotationLiteral "()" Nothing <$ reservedOp "()")
        <|> parens (
            do
                list <- sepBy1 typeAnnotationParser' $ reservedOp "->"
                return $ TypeAnnotationFunction (init list) (last list)
            )
        <|> (TypeAnnotationLiteral <$> word <*> (optionMaybe . parens . commaSep1 $ word) )
    word = many1 (letter <|> digit)


statementParser = varDeclarationParser <|> fmap StatementExpression (expressionParser <* semi)

varDeclarationParser = VariableDeclaration
    <$  reserved "var"
    <*> identifier
    <*> typeAnnotationParser
    <*> optionMaybe (reservedOp "=" >> expressionParser)
    <*  semi

expressionParser =
    let expressionParser' = parens expressionParser <|> structConstructionExpressionParser <|> literalExpressionParser <|> fmap ExpressionIdentifier identifier
    in buildExpressionParser exprtable expressionParser'

structConstructionExpressionParser = braces $ ExpressionStructConstruction <$> commaSep1 ((,) <$> identifier <* colon <*> expressionParser)

literalExpressionParser = fmap ExpressionLiteral $
    (reservedOp "()" >> pure LiteralEmptyTuple)
    <|> fmap LiteralInt natural
    <|> fmap LiteralFloat float
  --  <|> fmap (LiteralFloat . fromIntegral) (natural <* char 'f')

--epression parser table
exprtable = [
        [Prefix (ExpressionNegate <$ reservedOp "-")]
    ]

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
semiSep1 = P.semiSep1 lexer
symbol = P.symbol lexer
braces = P.braces lexer
reservedOp = P.reservedOp lexer
colon = P.colon lexer
natural = P.natural lexer
float = P.float lexer