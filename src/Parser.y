{
module Parser (parse) where
import Ast
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
    int       { ( pos, (TokenInt $$) ) }
    float     { ( pos, (TokenFloat $$) ) }
    word      { ( pos, (TokenWord $$) ) }
    stringlit { ( pos, (TokenStringLit $$) ) }
    tainted   { ( pos, (TokenTainted ) ) }
    pure      { ( pos, (TokenPure) ) }

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
    : ExpressionSequence { (UnknownType, UnknownPurity, ExpressionBlock $1) }
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
    : '{' ExpressionSequence '}' { (UnknownType, UnknownPurity, ExpressionBlock $2) }
    | ExpressionIf { $1 }
    | ExpressionLoop { $1 }
    ;

OtherExpression :: { Expression }
    : '(' Expression ')' { $2 }
    | Expression '(' ExpressionList ')' { (UnknownType, UnknownPurity, ExpressionFunctionCall $1 $3) }
    | pure Expression { (\(t,_,e) -> (t, TaintLevel 0, e)) $2 }
    | tainted Expression { (\(t,_,e) -> (t, InfiniteTaint,e)) $2 }
    | load Dotpath { (UnknownType, UnknownPurity, ExpressionLoad (init $2) (last $2)) }
    | return Expression { (UnknownType, UnknownPurity, ExpressionReturn $2) }
    | new word { (UnknownType, UnknownPurity, ExpressionNew $2) }
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
    : if Condition Expression else Expression { (UnknownType, UnknownPurity, ExpressionIf $2 $3 $5) }
    | if Condition Expression { (UnknownType, UnknownPurity, ExpressionIf $2 $3 emptyExpression) }
    ;

ExpressionLoop :: { Expression }
    : while Condition Expression { (UnknownType, UnknownPurity, ExpressionWhile $2 $3) }

ExpressionArith :: { Expression }
    : Expression '*' Expression { (UnknownType, UnknownPurity, ExpressionArithMul $1 $3) }
    | Expression '/' Expression { (UnknownType, UnknownPurity, ExpressionArithDiv $1 $3) }
    | Expression '+' Expression { (UnknownType, UnknownPurity, ExpressionArithPlus $1 $3) }
    | Expression '-' Expression { (UnknownType, UnknownPurity, ExpressionArithMinus $1 $3) }
    | Expression '%' Expression { (UnknownType, UnknownPurity, ExpressionArithMod $1 $3) }
    | Expression "++" { (UnknownType, UnknownPurity, ExpressionInc $1) }
    | Expression "--" { (UnknownType, UnknownPurity, ExpressionDec $1) }
    | '!' Expression { (UnknownType, UnknownPurity, ExpressionNot $2) }
    | Expression "&&" Expression { (UnknownType, UnknownPurity, ExpressionAnd $1 $3) }
    | Expression "||" Expression { (UnknownType, UnknownPurity, ExpressionOr $1 $3) }
    ;

ExpressionComp :: { Expression }
    : Expression "==" Expression { (UnknownType, UnknownPurity, ExpressionEq $1 $3) }
    | Expression "!=" Expression { (UnknownType, UnknownPurity, ExpressionNeq $1 $3) }
    | Expression '<' Expression { (UnknownType, UnknownPurity, ExpressionLt $1 $3) }
    | Expression "<=" Expression { (UnknownType, UnknownPurity, ExpressionLeq $1 $3) }
    | Expression '>' Expression { (UnknownType, UnknownPurity, ExpressionGt $1 $3) }
    | Expression ">=" Expression { (UnknownType, UnknownPurity, ExpressionGeq $1 $3) }
    ;

ExpressionConstant :: { Expression }
    : int { (TypeName "Int", UnknownPurity, ExpressionConstant $ ConstantInt $1) }
    | stringlit { (TypeName "String", UnknownPurity, ExpressionConstant $ ConstantString $1) }
    | true { (TypeName "Bool", UnknownPurity, ExpressionConstant $ ConstantBool True) }
    | false { (TypeName "Bool", UnknownPurity, ExpressionConstant $ ConstantBool False) }
    | float { (TypeName "Float", UnknownPurity, ExpressionConstant $ ConstantFloat $1) }
    ;

ExpressionAssign :: { Expression }
    : Identifier '=' Expression { (UnknownType, UnknownPurity, ExpressionAssign $1 $3) }
    | ExpressionVarDeclaration '=' Expression { (\(_,_,ExpressionVarDeclaration d _) e
        -> (TypeVoid, UnknownPurity, ExpressionVarDeclaration d e))  $1 $3 }

ExpressionLookup :: { Expression }
    : Identifier { (UnknownType, UnknownPurity, ExpressionLookup $1) }
    ;

Declarator :: { Declarator }
    : word { DeclaratorName $1 }
    | word ':' word { DeclaratorTyped $1 $3 }
    ;

DeclaratorList :: { [Declarator] }
    : Declarator { [$1] }
    | DeclaratorList ',' Declarator { $1 ++ [$3] }
    ;

ExpressionVarDeclaration :: { Expression }
    : var Declarator { (TypeVoid, UnknownPurity, ExpressionVarDeclaration $2 emptyExpression) }
    ;

FunctionTypeDeclarator :: { TypeDeclaration }
    : {-- empty --} { [] }
    | ':' word { $2 }
    ;

ExpressionFunctionDeclaration :: { Expression }
    : function word '(' DeclaratorList ')' FunctionTypeDeclarator Expression { (UnknownType, UnknownPurity, ExpressionNamedFunctionDeclaration $2 $4 $6 $7) }
    | function '(' DeclaratorList ')' FunctionTypeDeclarator Expression { (UnknownType, UnknownPurity, ExpressionAnonFunctionDeclaration $3 $5 $6) }
    | '\\' '(' DeclaratorList ')' "->" Expression %prec BACKSLASHDECL{ (UnknownType, UnknownPurity, ExpressionAnonFunctionDeclaration $3 [] $6) }
    ;

ExpressionDeclaration :: { Expression }
    : ExpressionVarDeclaration { $1 }
    | ExpressionFunctionDeclaration { $1 }
    | typedef word '{' DeclarationList '}' { (UnknownType, UnknownPurity, ExpressionTypedefDeclaration $2 $4) }
    ;

DeclarationList :: { [Expression] }
    : ExpressionDeclaration ';' { [$1] }
    | DeclarationList ExpressionDeclaration ';' { $1 ++ [$2] }
    ;

Identifier :: { Identifier }
    : word { IdentifierName $1 }
    | Identifier '.' word { IdentifierObjMember $1 $3 }
    | Identifier '[' Expression ']' { IdentifierArray $1 $3 }
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


}
