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
    if        { ( _ , TokenIf ) }
    else      { ( _ , TokenElse ) }
    while     { ( _ , TokenWhile ) }
    function  { ( _ , TokenFunction ) }
    return    { ( _ , TokenReturn ) }
    load      { ( _ , TokenLoad ) }
    typedef   { ( _ , TokenTypedef ) }
    new       { ( _ , TokenNew ) }
    var       { ( _ , TokenVar) }
    true      { ( _ , TokenTrue) }
    false     { ( _ , TokenFalse) }
    extends   { ( _ , TokenExtends ) }
    ';'       { ( _ , TokenSemicolon) }
    ':'       { ( _ , TokenColon) }
    ','       { ( _ , TokenComma ) }
    '.'       { ( _ , TokenDot ) }
    '('       { ( _ , TokenRBOpen ) }
    ')'       { ( _ , TokenRBClose ) }
    '{'       { ( _ , TokenCBOpen ) }
    '}'       { ( _ , TokenCBClose ) }
    '['       { ( _ , TokenSBOpen ) }
    ']'       { ( _ , TokenSBClose ) }
    '\\'      { ( _ , TokenBackslash ) }
    '='       { ( _ , TokenAssign ) }
    "=="      { ( _ , TokenCompEq ) }
    "!="      { ( _ , TokenCompNeq ) }
    '<'       { ( _ , TokenCompLt ) }
    "<="      { ( _ , TokenCompLeq ) }
    '>'       { ( _ , TokenCompGt ) }
    ">="      { ( _ , TokenCompGeq ) }
    '+'       { ( _ , TokenArithPlus ) }
    '-'       { ( _ , TokenArithMinus ) }
    '*'       { ( _ , TokenArithMul ) }
    '/'       { ( _ , TokenArithDiv ) }
    '%'       { ( _ , TokenArithMod ) }
    "++"      { ( _ , TokenArithInc ) }
    "--"      { ( _ , TokenArithDec ) }
    "&&"      { ( _ , TokenLogicAnd ) }
    "||"      { ( _ , TokenLogicOr ) }
    '!'       { ( _ , TokenLogicNot ) }
    "->"      { ( _ , TokenRightarrow ) }
    "String"  { ( _ , TokenStringType ) }
    "Int"     { ( _ , TokenIntType ) }
    "Float"   { ( _ , TokenFloatType ) }
    "Bool"    { ( _ , TokenBoolType ) }
    int       { tok @( _ , TokenInt $$) }
    float     { tok @( _ , TokenFloat $$) }
    word      { tok @( _ , TokenWord $$) }
    stringlit { tok @( _ , TokenStringLit $$) }
    tainted   { ( _ , TokenTainted ) }
    pure      { ( _ , TokenPure ) }

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
    : '{' ExpressionSequence '}' { liftExpression (ExpressionBlock $2) (fst $1) }
    | ExpressionIf { $1 }
    | ExpressionLoop { $1 }
    ;

OtherExpression :: { Expression }
    : '(' Expression ')' { $2 }
    | Expression '(' ExpressionList ')' { liftExpression (ExpressionFunctionCall $1 $3) (getExpPos $1)}
    | pure Expression { (\(t,(_,i),e,p) -> (t, (TaintLevel 0,i), e,p)) $2 }
    | tainted Expression { (\(t,(_,i),e,p) -> (t, (InfiniteTaint,i),e,p)) $2 }
    | load Dotpath { liftExpression (ExpressionLoad (init $2) (last $2)) (fst $1) }
    | return Expression { liftExpression (ExpressionReturn $2) (fst $1) }
    | new word { liftExpression (ExpressionNew $2) (fst $1) }
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
    : if Condition Expression else Expression { liftExpression (ExpressionIf $2 $3 $5) (fst $1) }
    | if Condition Expression { liftExpression (ExpressionIf $2 $3 emptyExpression) (fst $1) }
    ;

ExpressionLoop :: { Expression }
    : while Condition Expression { liftExpression (ExpressionWhile $2 $3) (fst $1) }

ExpressionArith :: { Expression }
    : Expression '*' Expression  { liftExpression (ExpressionArithMul $1 $3) (fst $2) }
    | Expression '/' Expression  { liftExpression (ExpressionArithDiv $1 $3) (fst $2) }
    | Expression '+' Expression  { liftExpression (ExpressionArithPlus $1 $3) (fst $2) }
    | Expression '-' Expression  { liftExpression (ExpressionArithMinus $1 $3) (fst $2) }
    | Expression '%' Expression  { liftExpression (ExpressionArithMod $1 $3) (fst $2) }
    | ExpressionLookup "++"      { liftExpression (ExpressionInc $1) (fst $2) }
    | ExpressionLookup "--"      { liftExpression (ExpressionDec $1) (fst $2) }
    | '!' Expression             { liftExpression (ExpressionNot $2) (fst $1) }
    | Expression "&&" Expression { liftExpression (ExpressionAnd $1 $3) (fst $2) }
    | Expression "||" Expression { liftExpression (ExpressionOr $1 $3) (fst $2) }
    ;

ExpressionComp :: { Expression }
    : Expression "==" Expression { liftExpression (ExpressionEq $1 $3) (fst $2) }
    | Expression "!=" Expression { liftExpression (ExpressionNeq $1 $3) (fst $2) }
    | Expression '<' Expression  { liftExpression (ExpressionLt $1 $3) (fst $2) }
    | Expression "<=" Expression { liftExpression (ExpressionLeq $1 $3) (fst $2) }
    | Expression '>' Expression  { liftExpression (ExpressionGt $1 $3) (fst $2) }
    | Expression ">=" Expression { liftExpression (ExpressionGeq $1 $3) (fst $2) }
    ;

ExpressionConstant :: { Expression }
    : int       { setExpPos (fst tok) (astConstant TypeInt $ ConstantInt $1) }
    | stringlit { setExpPos (fst tok) (astConstant TypeString $ ConstantString $1) }
    | true      { setExpPos (fst $1) (astConstant TypeBool $ ConstantBool True) }
    | false     { setExpPos (fst $1) (astConstant TypeBool $ ConstantBool False) }
    | float     { setExpPos (fst tok) (astConstant TypeFloat $ ConstantFloat $1) }
    ;

ExpressionAssign :: { Expression }
    : Identifier '=' Expression { liftExpression (ExpressionAssign (fst $1) $3) (fst $2)}
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
    : var Declarator { (TypeVoid, defaultCompilerinfo, ExpressionVarDeclaration $2 emptyExpression, fst $1) }
    ;

FunctionTypeDeclarator :: { ExprType }
    : {-- empty --} { UnknownType }
    | ':' TypeDeclaration { $2 }
    ;

ExpressionFunctionDeclaration :: { Expression }
    : function word '(' DeclaratorList ')' FunctionTypeDeclarator Expression
        {
        setType (TypeFunction $ decllistToExptypelist $4) $ liftExpression (ExpressionNamedFunctionDeclaration $2 $4 $6 $7) (fst $1)
        }
    | function '(' DeclaratorList ')' FunctionTypeDeclarator Expression
        {
        setType (TypeFunction $ decllistToExptypelist $3) $ liftExpression (ExpressionAnonFunctionDeclaration $3 $5 $6) (fst $1)
        }
    | '\\' DeclaratorList "->" Expression %prec BACKSLASHDECL
        {
        setType (TypeFunction $ decllistToExptypelist $2) $ liftExpression (ExpressionAnonFunctionDeclaration $2 UnknownType $4) (fst $1)
        }
    ;

ExpressionDeclaration :: { Expression }
    : ExpressionVarDeclaration { $1 }
    | ExpressionFunctionDeclaration { $1 }
    | typedef word '{' DeclarationList '}' { liftExpression (ExpressionTypedefDeclaration $2 $4) (fst $1) }
    ;

DeclarationList :: { [Expression] }
    : ExpressionDeclaration ';' { [$1] }
    | DeclarationList ExpressionDeclaration ';' { $1 ++ [$2] }
    ;

Identifier :: { (Identifier,LexerPosition) }
    : word { (IdentifierName $1, fst tok) }
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


decllistToMap :: [Declarator] -> Map.Map String ExprType
decllistToMap a = Map.fromList $ map (\(Declarator a b) -> (a,b)) a

decllistToExptypelist :: [Declarator] -> [ExprType]
decllistToExptypelist = map (\ (Declarator _ t) -> t)

}
