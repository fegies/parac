{
module Parser (parse) where
import Ast.Expression
import Ast.Type
import qualified Data.Map as Map
import Tokens
}

%name parse
%tokentype { LexToken }
%error { parseError }



%token
    if        { ( pos, TokenIf ) }
    else      { ( pos, TokenElse ) }
    while     { ( pos, TokenWhile ) }
    function  { ( pos, TokenFunction ) }
    return    { ( pos, TokenReturn ) }
    load      { ( pos, TokenLoad ) }
    typedef   { ( pos, TokenTypedef ) }
    new       { ( pos, TokenNew ) }
    var       { ( pos, TokenVar) }
    true      { ( pos, TokenTrue) }
    false     { ( pos, TokenFalse) }
    extends   { ( pos, TokenExtends ) }
    ';'       { ( pos, TokenSemicolon) }
    ':'       { ( pos, TokenColon) }
    ','       { ( pos, TokenComma ) }
    '.'       { ( pos, TokenDot ) }
    '('       { ( pos, TokenRBOpen ) }
    ')'       { ( pos, TokenRBClose ) }
    '{'       { ( pos, TokenCBOpen ) }
    '}'       { ( pos, TokenCBClose ) }
    '['       { ( pos, TokenSBOpen ) }
    ']'       { ( pos, TokenSBClose ) }
    '\\'      { ( pos, TokenBackslash ) }
    '='       { ( pos, TokenAssign ) }
    "=="      { ( pos, TokenCompEq ) }
    "!="      { ( pos, TokenCompNeq ) }
    '<'       { ( pos, TokenCompLt ) }
    "<="      { ( pos, TokenCompLeq ) }
    '>'       { ( pos, TokenCompGt ) }
    ">="      { ( pos, TokenCompGeq ) }
    '+'       { ( pos, TokenArithPlus ) }
    '-'       { ( pos, TokenArithMinus ) }
    '*'       { ( pos, TokenArithMul ) }
    '/'       { ( pos, TokenArithDiv ) }
    '%'       { ( pos, TokenArithMod ) }
    "++"      { ( pos, TokenArithInc ) }
    "--"      { ( pos, TokenArithDec ) }
    "&&"      { ( pos, TokenLogicAnd ) }
    "||"      { ( pos, TokenLogicOr ) }
    '!'       { ( pos, TokenLogicNot ) }
    "->"      { ( pos, TokenRightarrow ) }
    "String"  { ( pos, TokenStringType ) }
    "Int"     { ( pos, TokenIntType ) }
    "Float"   { ( pos, TokenFloatType ) }
    "Bool"    { ( pos, TokenBoolType ) }
    int       { tok @( pos, TokenInt $$) }
    float     { tok @( pos, TokenFloat $$) }
    word      { tok @( pos, TokenWord $$) }
    stringlit { tok @( pos, TokenStringLit $$) }
    tainted   { ( pos, TokenTainted ) }
    pure      { ( pos, TokenPure ) }

%nonassoc BACKSLASHDECL
%nonassoc pure tainted
%nonassoc '{' '}'
%nonassoc return
%nonassoc if while
%nonassoc ';'
%left ','
%right '='
%left "&&" "||"
%left "==" "!="
%left '>' ">="
%left '<' "<="
%left '+' '-'
%left '*' '/' '%'
%right '!'
%left '.'
%left '[' ']'
%left '(' ')'
%left "++" "--"
%nonassoc else

%%


Program :: { Expression }
    : ExpressionSequence { liftExpression (ExpressionBlock $1) (0,0,0) }
    ;

ExpressionSequence :: { [Expression] }
    : OtherExpression ';'    { [$1] }
    | ExpressionSequence OtherExpression ';' { $1 ++ [$2] }
    | OptionalSemicolonExpression ExpressionTerminator { [$1] }
    | ExpressionSequence OptionalSemicolonExpression ExpressionTerminator { $1 ++ [$2] }
    ;

ExpressionTerminator :: { () }
    : ';'   { () }
    | error { () }
    ;

ExpressionList :: { [Expression] }
    : Expression { [$1] }
    | ExpressionList ',' Expression { $1 ++ [$3] }
    ;

Expression :: { Expression }
    : OptionalSemicolonExpression { $1 }
    | OtherExpression { $1 }
    ;

OptionalSemicolonExpression :: { Expression }
    : '{' ExpressionSequence '}' { liftExpression (ExpressionBlock $2) (getTokPos $1) }
    | ExpressionIf { $1 }
    | ExpressionLoop { $1 }
    ;

OtherExpression :: { Expression }
    : '(' Expression ')' { $2 }
    | Expression '(' ExpressionList ')' { liftExpression (ExpressionFunctionCall $1 $3) (getExpPos $1)}
    | pure Expression { (\(t,(_,i),e,p) -> (t, (TaintLevel 0,i), e,p)) $2 }
    | tainted Expression { (\(t,(_,i),e,p) -> (t, (InfiniteTaint,i),e,p)) $2 }
    | load Dotpath { liftExpression (ExpressionLoad (init $2) (last $2)) (getTokPos $1) }
    | return Expression { liftExpression (ExpressionReturn $2) (getTokPos $1) }
    | new word { liftExpression (ExpressionNew $2) (getTokPos $1) }
    | ExpressionArith { $1 }
    | ExpressionLookup { $1 }
    | ExpressionAssign { $1 }
    | ExpressionDeclaration { $1 }
    | ExpressionConstant { $1 }
    | ExpressionComp { $1 }
    ;

Condition :: { Expression }
    : '(' Expression ')' { $2 }
    ;

ExpressionIf :: { Expression }
    : if Condition Expression else Expression { liftExpression (ExpressionIf $2 $3 $5) (getTokPos $1) }
    | if Condition Expression { liftExpression (ExpressionIf $2 $3 emptyExpression) (getTokPos $1) }
    ;

ExpressionLoop :: { Expression }
    : while Condition Expression { liftExpression (ExpressionWhile $2 $3) (getTokPos $1) }

ExpressionArith :: { Expression }
    : Expression '*' Expression  { liftExpression (ExpressionArithMul $1 $3) (getTokPos $2) }
    | Expression '/' Expression  { liftExpression (ExpressionArithDiv $1 $3) (getTokPos $2) }
    | Expression '+' Expression  { liftExpression (ExpressionArithPlus $1 $3) (getTokPos $2) }
    | Expression '-' Expression  { liftExpression (ExpressionArithMinus $1 $3) (getTokPos $2) }
    | Expression '%' Expression  { liftExpression (ExpressionArithMod $1 $3) (getTokPos $2) }
    | ExpressionLookup "++"      { liftExpression (ExpressionInc $1) (getTokPos $2) }
    | ExpressionLookup "--"      { liftExpression (ExpressionDec $1) (getTokPos $2) }
    | '!' Expression             { liftExpression (ExpressionNot $2) (getTokPos $1) }
    | Expression "&&" Expression { liftExpression (ExpressionAnd $1 $3) (getTokPos $2) }
    | Expression "||" Expression { liftExpression (ExpressionOr $1 $3) (getTokPos $2) }
    ;

ExpressionComp :: { Expression }
    : Expression "==" Expression { liftExpression (ExpressionEq $1 $3) (getTokPos $2) }
    | Expression "!=" Expression { liftExpression (ExpressionNeq $1 $3) (getTokPos $2) }
    | Expression '<' Expression  { liftExpression (ExpressionLt $1 $3) (getTokPos $2) }
    | Expression "<=" Expression { liftExpression (ExpressionLeq $1 $3) (getTokPos $2) }
    | Expression '>' Expression  { liftExpression (ExpressionGt $1 $3) (getTokPos $2) }
    | Expression ">=" Expression { liftExpression (ExpressionGeq $1 $3) (getTokPos $2) }
    ;

ExpressionConstant :: { Expression }
    : int       { setExpPos (getTokPos tok) (astConstant TypeInt $ ConstantInt $1) }
    | stringlit { setExpPos (getTokPos tok) (astConstant TypeString $ ConstantString $1) }
    | true      { setExpPos (getTokPos $1) (astConstant TypeBool $ ConstantBool True) }
    | false     { setExpPos (getTokPos $1) (astConstant TypeBool $ ConstantBool False) }
    | float     { setExpPos (getTokPos tok) (astConstant TypeFloat $ ConstantFloat $1) }
    ;

ExpressionAssign :: { Expression }
    : Identifier '=' Expression { liftExpression (ExpressionAssign (fst $1) $3) (getTokPos $2)}
    | ExpressionVarDeclaration '=' Expression { (\(_,_,ExpressionVarDeclaration d _, p) e
        -> (TypeVoid, defaultCompilerinfo, ExpressionVarDeclaration d e, p))  $1 $3 }

ExpressionLookup :: { Expression }
    : Identifier { liftExpression (ExpressionLookup $ fst $1) (snd $1)}
    ;

TypeDeclaration :: { ExprType }
    : "String" { TypeString }
    | "Int" { TypeInt }
    | "Float" { TypeFloat }
    | "Bool" { TypeBool }
    | '[' TypeDeclaration ']' { TypeArray $2 }
    | '{' DeclaratorList '}' { TypeTypedef . decllistToMap $ $2 }
    | '(' ArrowSeparetedList ')' { TypeFunction $2 }
    | word { TypeUnresolved $1 }
    ;

ArrowSeparetedList :: { [ExprType] }
    : TypeDeclaration { [$1] }
    | ArrowSeparetedList "->" TypeDeclaration { $1 ++ [$3] }
    ;

Declarator :: { Declarator }
    : word { Declarator $1 UnknownType }
    | word ':' TypeDeclaration { Declarator $1 $3 }
    ;

DeclaratorList :: { [Declarator] }
    : Declarator { [$1] }
    | DeclaratorList ',' Declarator { $1 ++ [$3] }
    ;

ExpressionVarDeclaration :: { Expression }
    : var Declarator { (TypeVoid, defaultCompilerinfo, ExpressionVarDeclaration $2 emptyExpression, getTokPos $1) }
    ;

FunctionTypeDeclarator :: { ExprType }
    : {-- empty --} { UnknownType }
    | ':' TypeDeclaration { $2 }
    ;

ExpressionFunctionDeclaration :: { Expression }
    : function word '(' DeclaratorList ')' FunctionTypeDeclarator Expression
        {
        setType (TypeFunction $ decllistToExptypelist $4) $ liftExpression (ExpressionNamedFunctionDeclaration $2 $4 $6 $7) (getTokPos $1)
        }
    | function '(' DeclaratorList ')' FunctionTypeDeclarator Expression
        {
        setType (TypeFunction $ decllistToExptypelist $3) $ liftExpression (ExpressionAnonFunctionDeclaration $3 $5 $6) (getTokPos $1)
        }
    | '\\' DeclaratorList "->" Expression %prec BACKSLASHDECL
        {
        setType (TypeFunction $ decllistToExptypelist $2) $ liftExpression (ExpressionAnonFunctionDeclaration $2 UnknownType $4) (getTokPos $1)
        }
    ;

ExpressionDeclaration :: { Expression }
    : ExpressionVarDeclaration { $1 }
    | ExpressionFunctionDeclaration { $1 }
    | typedef word '{' DeclarationList '}' { liftExpression (ExpressionTypedefDeclaration $2 $4) (getTokPos $1) }
    ;

DeclarationList :: { [Expression] }
    : ExpressionDeclaration ';' { [$1] }
    | DeclarationList ExpressionDeclaration ';' { $1 ++ [$2] }
    ;

Identifier :: { (Identifier,LexerPosition) }
    : word { (IdentifierName $1, getTokPos tok) }
    | Identifier '.' word { (IdentifierObjMember (fst $1) $3, snd $1) }
    | Identifier '[' Expression ']' { (IdentifierArray (fst $1) $3, snd $1) }
    ;

Dotpath :: { [String] }
    : word { [$1] }
    | '.' '.' { [".."] }
    | Dotpath '.' '.' '.' { $1 ++ [".."] }
    | Dotpath '.' word { $1 ++ [$3] }
    ;
{



reportPos :: LexerPosition -> String
reportPos (_, l, c) = "line "++ show l ++ ", column " ++ show c

reportError :: Token -> String
reportError a = "unexpected token: "++show a

parseError :: [LexToken] -> a
parseError [] = error "parse error: unexpected eof, did you forget a closing brace or semicolon?"
parseError ((pos,tok):_) = error $ "parse Error at: "++ reportPos pos ++ "\n"
    ++ reportError tok

setExpPos :: LexerPosition -> Expression -> Expression
setExpPos p (t,i,b,_) = (t,i,b,p)

getExpPos :: Expression -> LexerPosition
getExpPos (_,_,_,p) = p

getTokPos :: LexToken -> LexerPosition
getTokPos (p,_) = p

decllistToMap :: [Declarator] -> Map.Map String ExprType
decllistToMap a = Map.fromList $ map (\(Declarator a b) -> (a,b)) a

decllistToExptypelist :: [Declarator] -> [ExprType]
decllistToExptypelist = map (\ (Declarator _ t) -> t)

}
