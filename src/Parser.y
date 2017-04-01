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
    "typedef" { ( pos, TokenTypedef ) }
    new       { ( pos, TokenNew ) }
    var       { ( pos, TokenVar $$) }
    ';'       { ( pos, TokenSemicolon) }
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
    int       { ( pos, (TokenInt $$) ) }
    word      { ( pos, (TokenWord $$) ) }
    stringlit { ( pos, (TokenStringLit $$) ) }
    tainted   { ( pos, (TokenTainted ) ) }
    pure      { ( pos, (TokenPure) ) }

%nonassoc EMPTYE
%nonassoc '{' '}'
%nonassoc ';'
%nonassoc pure tainted
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

%%


Program :: { Expression }
    : NonemptyExpressionSequence { ExpressionBlock $1 }
    ;


Block :: { Expression }
    : TerminatedExpression       {  $1 }
    | BracedSequence {  $1 }
    | '{' '}'     { EmptyExpression }
    ;

BracedSequence :: { Expression }
    : '{' NonemptyExpressionSequence '}' { ExpressionBlock $2 }
    ;

NonemptyExpressionSequence :: { [Expression] }
    : TerminatedExpression  { [$1] }
    | NonemptyExpressionSequence TerminatedExpression { $1 ++ [$2] }
    ;

TerminatedExpression :: { Expression }
    : return NonterminatedExpression ';'        { ExpressionReturn $2 }
    | ExpressionDeclaration                     { $1 }
    | load stringlit                            { ExpressionLoad $2 }
    | NonterminatedExpression ';'               { $1 }
    | ExpressionWhile                           { $1 }
    | pure TerminatedExpression                 { SESetExpression True $2 }
    | tainted TerminatedExpression              { SESetExpression False $2 }
    ;

ExpressionDeclaration :: { Expression }
    : var word                                  { ExpressionVarDeclaration $2 }
    | function word '(' WordList ')' Block      { ExpressionNamedFunctionDeclaration $2 $4 $6 }
    | "typedef" word '{' DeclarationList '}' ';'{ ExpressionTypedefDeclaration $2 $4 }
    ;

NonterminatedExpression :: { Expression }
    : function '(' WordList ')' Block                { ExpressionAnonFunctionDeclaration $3 $5 }
    | NonterminatedExpression '(' ExpressionList ')' { ExpressionFunctionCall $1 $3 }
    | ExpressionIf                                   { $1 }
    | '(' NonterminatedExpression ')'                { $2 }
    | ExpressionArith                                { $1 }
    | ExpressionComp                                 { $1 }
    | ExpressionConstant                             { $1 }
    | ExpressionAssign                               { $1 }
    | ExpressionLookup                               { $1 }
    | new word                                       { ExpressionNew $2 }
    | pure NonterminatedExpression                   { SESetExpression True $2 }
    | tainted NonterminatedExpression                { SESetExpression False $2 }
    ;

ExpressionList :: { [Expression] }
    : NonterminatedExpression { [$1] }
    | ExpressionList ',' NonterminatedExpression { $1 ++ [$3] }
    ;

Condition :: { Expression }
    : '(' NonterminatedExpression ')' { $2 }
    ;

ExpressionIf :: { Expression }
    : if Condition Block else Block { ExpressionIf $2 $3 $5 }
    | if Condition Block            { ExpressionIf $2 $3 EmptyExpression }
    ;

ExpressionWhile :: { Expression }
    : while Condition Block { ExpressionWhile $2 $3 }
    ;

ExpressionArith :: { Expression }
    : NonterminatedExpression '+' NonterminatedExpression  { ExpressionArithPlus $1 $3 }
    | NonterminatedExpression '-' NonterminatedExpression  { ExpressionArithMinus $1 $3 }
    | NonterminatedExpression '*' NonterminatedExpression  { ExpressionArithMul $1 $3 }
    | NonterminatedExpression '/' NonterminatedExpression  { ExpressionArithDiv $1 $3 }
    | NonterminatedExpression '%' NonterminatedExpression  { ExpressionArithMod $1 $3 }
    | NonterminatedExpression "++"                         { ExpressionInc $1 }
    | NonterminatedExpression "--"                         { ExpressionDec $1 }
    | '!' NonterminatedExpression                          { ExpressionNot $2 }
    | NonterminatedExpression "&&" NonterminatedExpression { ExpressionAnd $1 $3 }
    | NonterminatedExpression "||" NonterminatedExpression { ExpressionOr $1 $3 }
    ;

ExpressionComp :: { Expression }
    : NonterminatedExpression "==" NonterminatedExpression { ExpressionEq $1 $3 }
    | NonterminatedExpression "!=" NonterminatedExpression { ExpressionNeq $1 $3 }
    | NonterminatedExpression '<' NonterminatedExpression  { ExpressionLt $1 $3 }
    | NonterminatedExpression "<=" NonterminatedExpression { ExpressionLeq $1 $3 }
    | NonterminatedExpression '>' NonterminatedExpression  { ExpressionGt $1 $3 }
    | NonterminatedExpression ">=" NonterminatedExpression { ExpressionGeq $1 $3 }
    ;

ExpressionConstant :: { Expression }
    : int       { ExpressionConstant $ ConstantInt $1 }
    | stringlit { ExpressionConstant $ ConstantString $1 }
    ;

DeclarationList :: { [Expression] }
    : ExpressionDeclaration ';'                 { [$1] }
    | DeclarationList ExpressionDeclaration ';' { $1 ++ [$2] }
    ;

WordList :: { [String] }
    : word              { [$1] }
    | WordList ',' word { $1 ++ [$3] }
    ;

Identifier :: { Identifier }
    : word                                       { IdentifierName $1 }
    | Identifier '.' word                        { IdentifierObjMember $1 $3 }
    | Identifier '[' NonterminatedExpression ']' { IdentifierArray $1 $3 }
    ;

ExpressionLookup :: { Expression }
    : Identifier    { ExpressionLookup $1 }
    ;

ExpressionAssign :: { Expression }
    : Identifier '=' NonterminatedExpression { ExpressionAssign $1 $3 }
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
