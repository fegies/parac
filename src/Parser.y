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
    word      { ( pos, (TokenWord $$) ) }
    stringlit { ( pos, (TokenStringLit $$) ) }
    tainted   { ( pos, (TokenTainted ) ) }
    pure      { ( pos, (TokenPure) ) }

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
    : ExpressionSequence { ExpressionBlock $1 }
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
    : '{' ExpressionSequence '}' { ExpressionBlock $2 }
    | ExpressionIf { $1 }
    | ExpressionLoop { $1 }
    ;

OtherExpression :: { Expression }
    : '(' Expression ')' { $2 }
    | Expression '(' ExpressionList ')' { ExpressionFunctionCall $1 $3 }
    | pure Expression { SESetExpression True $2 }
    | tainted Expression { SESetExpression False $2 }
    | load Dotpath { ExpressionLoad (init $2) (last $2) }
    | return Expression { ExpressionReturn $2 }
    | new word { ExpressionNew $2 }
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
    : if Condition Expression else Expression { ExpressionIf $2 $3 $5 }
    | if Condition Expression { ExpressionIf $2 $3 EmptyExpression }
    ;

ExpressionLoop :: { Expression }
    : while Condition Expression { ExpressionWhile $2 $3 }

ExpressionArith :: { Expression }
    : Expression '*' Expression { ExpressionArithMul $1 $3 }
    | Expression '/' Expression { ExpressionArithDiv $1 $3 }
    | Expression '+' Expression { ExpressionArithPlus $1 $3 }
    | Expression '-' Expression { ExpressionArithMinus $1 $3 }
    | Expression '%' Expression { ExpressionArithMod $1 $3 }
    | Expression "++" { ExpressionInc $1 }
    | Expression "--" { ExpressionDec $1 }
    | '!' Expression { ExpressionNot $2 }
    | Expression "&&" Expression { ExpressionAnd $1 $3 }
    | Expression "||" Expression { ExpressionOr $1 $3 }
    ;

ExpressionComp :: { Expression }
    : Expression "==" Expression { ExpressionEq $1 $3 }
    | Expression "!=" Expression { ExpressionNeq $1 $3 }
    | Expression '<' Expression { ExpressionLt $1 $3 }
    | Expression "<=" Expression { ExpressionLeq $1 $3 }
    | Expression '>' Expression { ExpressionGt $1 $3 }
    | Expression ">=" Expression { ExpressionGeq $1 $3 }
    ;

ExpressionConstant :: { Expression }
    : int { ExpressionConstant $ ConstantInt $1 }
    | stringlit { ExpressionConstant $ ConstantString $1 }
    ;

ExpressionAssign :: { Expression }
    : Identifier '=' Expression { ExpressionAssign $1 $3 }
    | ExpressionVarDeclaration '=' Expression { ExpressionVarDeclaration (getExpVarDecDecl $1) $3 }

ExpressionLookup :: { Expression }
    : Identifier { ExpressionLookup $1 }
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
    : var Declarator { ExpressionVarDeclaration $2 EmptyExpression }
    ;

ExpressionDeclaration :: { Expression }
    : ExpressionVarDeclaration { $1 }
    | function word '(' DeclaratorList ')' Expression { ExpressionNamedFunctionDeclaration $2 $4 $6 }
    | function '(' DeclaratorList ')' Expression { ExpressionAnonFunctionDeclaration $3 $5 }
    | typedef word '{' DeclarationList '}' { ExpressionTypedefDeclaration $2 $4 }
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
