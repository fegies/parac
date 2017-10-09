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
    pos <- getPosition
    (name, args, rettype) <- functionSignatureParser
    body <- braces . many1 $ statementParser
    return $ FunctionDeclaration pos name args rettype body

dataDeclarationBodyParser constructor = do
    pos <- getPosition
    name <- lexeme upperWord
    (constraints, vars) <- typeVarsParser
    fields <- fieldsParser
    return $ constructor pos name constraints vars fields
    where
        fieldsParser = braces . commaSep1 $ (,) <$> identifier <*> typeAnnotationParser
typeDeclarationParser = reserved "data" >> dataDeclarationBodyParser DataDeclaration
enumDeclarationParser = reserved "enum" >> dataDeclarationBodyParser EnumDeclaration

classDeclarationParser = do
    reserved "class"
    pos <- getPosition
    name <- lexeme upperWord
    (constraints, vars) <- typeVarsParser
    fields <- braces . commaSep1 $ do
        signature <- functionSignatureParser
        definition <- optionMaybe . braces . many1 $ statementParser
        return (stripArgNames signature, fmap ((,) $ getFullArgs signature) definition)
    return $ ClassDeclaration pos name constraints vars fields
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


statementParser = varDeclarationParser <|> (StatementExpression <$> getPosition <*> expressionParser <* semi)

varDeclarationParser = VariableDeclaration
    <$  reserved "var"
    <*> getPosition
    <*> identifier
    <*> typeAnnotationParser
    <*> optionMaybe (reservedOp "=" >> expressionParser)
    <*  semi

expressionParser =
    let expressionParser'
            =   parens expressionParser
            <|> literalExpressionParser
            <|> structConstructionExpressionParser
            <|> brackets (ExpressionArrayConstruction <$> getPosition <*> commaSep expressionParser)
            <|> (ExpressionIdentifier <$> getPosition <*> identifier)
    in buildPrattParser exprtable expressionParser'

structConstructionExpressionParser = braces $ ExpressionStructConstruction <$> getPosition <*> commaSep1 ((,) <$> identifier <* colon <*> expressionParser)

literalExpressionParser = ExpressionLiteral <$> getPosition <*> (
    (reservedOp "()" >> pure LiteralEmptyTuple)
    <|> fmap LiteralInt natural
    <|> fmap LiteralFloat float
  --  <|> fmap (LiteralFloat . fromIntegral) (natural <* char 'f')
    )

--epression parser table
exprtable = [
    [Postfix $ ExpressionMonop <$> getPosition <*> pure Increment <* reservedOp "++", Postfix $ ExpressionMonop <$> getPosition <*> pure Decrement <* reservedOp "--"],
    [Postfix $ flip <$> fmap ExpressionDotOperator getPosition <* dot <*> identifier],
    [Postfix $ flip <$> fmap ExpressionFunctionCall getPosition <*> (parens . commaSep) expressionParser ],
    [Postfix $ (flip <$> (ExpressionBinop <$> getPosition <*> pure ArrayAccess)) <*> brackets expressionParser],
    [Prefix $ ExpressionMonop <$> getPosition <*> pure Negate <* reservedOp "-"],
    [Prefix $ ExpressionMonop <$> getPosition <*> pure OpNot <* reservedOp "!"],
    leftassocBinop [(ArithMul,"*"), (ArithDiv, "/"), (ArithMod, "%")],
    leftassocBinop [(ArithPlus, "+"), (ArithMinus, "-")],
    leftassocBinop [(CompareLt,"<"),(CompareLeq, "<="),(CompareGt,">"),(CompareGeq,">=")],
    leftassocBinop [(CompareEq, "=="), (CompareNeq, "!=") ],
    leftassocBinop [(OpAnd, "&&"), (OpOr, "||")],
    [Infix (ExpressionBinop <$> getPosition <*> pure Assign <* reservedOp "=") AssocRight ]
    ]
    where
        leftassocBinop = map $ uncurry (\a b -> Infix (ExpressionBinop <$> getPosition <*> pure a <* reservedOp b) AssocLeft)

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
brackets = P.brackets lexer
dot = P.dot lexer

-- Pratt Expression Parser by stackoverflow user Pat
-- https://stackoverflow.com/questions/33214163/parsec-expr-repeated-prefix-with-different-priority/33534426#33534426
buildPrattParser table termP = parser precs where
    
      precs = reverse table
    
      prefixP = choice prefixPs <|> termP where
        prefixPs = do
          precsR@(ops:_) <- tails precs 
          Prefix opP <- ops
          return $ opP <*> parser precsR
    
      infixP precs lhs = choice infixPs <|> pure lhs where
        infixPs = do
          precsR@(ops:precsL) <- tails precs
          op <- ops
          p <- case op of
            Infix opP assoc -> do
              let p precs = opP <*> pure lhs <*> parser precs
              return $ case assoc of
                AssocNone  -> error "Non associative operators are not supported"
                AssocLeft  -> p precsL
                AssocRight -> p precsR
            Postfix opP ->
              return $ opP <*> pure lhs
            Prefix _ -> mzero
          return $ p >>= infixP precs
    
      parser precs = prefixP >>= infixP precs
    