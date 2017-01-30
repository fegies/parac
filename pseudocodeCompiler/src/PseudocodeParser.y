{
module PseudocodeParser (parsePSC) where
import Ast
import Tokens
}

%name parsePSC
%tokentype { LexToken }
%error { parseError }



%token
    if        { PToken pos TokenIf }
    then      { PToken pos TokenThen }
    else      { PToken pos TokenElse }
    fi        { PToken pos TokenFi }
    while     { PToken pos TokenWhile }
    do        { PToken pos TokenDo }
    od        { PToken pos TokenOd }
    repeat    { PToken pos TokenRepeat }
    until     { PToken pos TokenUntil }
    for       { PToken pos TokenFor }
    to        { PToken pos TokenTo }
    downto    { PToken pos TokenDownto }
    function  { PToken pos TokenFunction }
    return    { PToken pos TokenReturn }
    "class"   { PToken pos TokenClass }
    new       { PToken pos TokenNew }
    ';'       { PToken pos TokenSemicolon}
    ','       { PToken pos TokenComma }
    '.'       { PToken pos TokenDot }
    '('       { PToken pos TokenRBOpen }
    ')'       { PToken pos TokenRBClose }
    '{'       { PToken pos TokenCBOpen }
    '}'       { PToken pos TokenCBClose }
    '['       { PToken pos TokenSBOpen }
    ']'       { PToken pos TokenSBClose }
    "<-"      { PToken pos TokenLeftarrow }
    "=="      { PToken pos TokenCompEq }
    "!="      { PToken pos TokenCompNeq }
    '<'       { PToken pos TokenCompLt }
    "<="      { PToken pos TokenCompLeq }
    '>'       { PToken pos TokenCompGt }
    ">="      { PToken pos TokenCompGeq }
    '+'       { PToken pos TokenArithPlus }
    '-'       { PToken pos TokenArithMinus }
    '*'       { PToken pos TokenArithMul }
    '/'       { PToken pos TokenArithDiv }
    '%'       { PToken pos TokenArithMod }
    "++"      { PToken pos TokenArithInc }
    "--"      { PToken pos TokenArithDec }
    "&&"      { PToken pos TokenLogicAnd }
    "||"      { PToken pos TokenLogicOr }
    '!'       { PToken pos TokenLogicNot }
    int       { PToken pos (TokenInt $$) }
    word      { PToken pos (TokenWord $$) }
    stringlit { PToken pos (TokenStringLit $$) }

%left ','
%right "<-"
%left "&&"
%left "||"
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


Program :: { Program }
    : Stmts { $1 }

Block :: { Block }
Block : {- empty -}         { [] }
      | Statement           { $1:[] }
      | '{' Stmts '}'       { $2 }

Stmts :: { Block }
      : Statement { $1:[] }
      | Stmts Statement { $1 ++ $2:[] }

Statement :: { Statement }
    : StatementIf { $1 }
    | StatementLoop { $1 }
    | return ';'    { StatementReturn EmptyExpression }
    | return Expression ';' { StatementReturn $2 }
    | function word '(' FunctionParams ')' Block{ StatementFunctionDeclaration $2 $4 $6}
    | "class" word '{' ClassParams '}' { StatementClassDeclaration $2 $4}
    | Expression ';' { StatementExpression $1 }

ClassParams :: { [String] }
    : {-empty -} { [] }
    | word ';'      { $1:[] }
    | ClassParams word ';' { $1 ++ $2:[] }


FunctionParams :: { [String] }
    : {-empty -} { [] }
    | word       { $1:[] }
    | FunctionParams ',' word { $1 ++ $3:[] }

StatementIf :: { Statement }
    : if Expression ';' then Block else Block fi { StatementIf $2 $5 $7 }
    | if Expression ';' then Block fi { StatementIf $2 $5 [] }

StatementLoop :: { Statement }
    : while Expression ';' do Block od { StatementWhile $2 $5 }
    | repeat Block until Expression ';' { StatementRepeat $2 $4 }
    | for Expression to Expression ';' do Block od { StatementForTo $2 $4 $7}
    | for Expression downto Expression ';' do Block od { StatementForDownto $2 $4 $7}

Expression :: { Expression }
    : stringlit { ExpressionConstant (ConstantString $1) }
    | Expression '(' FunctionArguments ')' {ExpressionFunctionCall $1 $3}
    | Expression '[' Expression ']' { ExpressionArrayAccess $1 $3 }
    | Expression '.' word { ExpressionObjectMembAccess $1 $3 }
    | int { ExpressionConstant (ConstantInt $1) }
    | '(' Expression ')' { $2 }
    | ExpressionArithmetic { $1 }
    | ExpressionLogic { $1 }
    | ExpressionCompare { $1 }
    | ExpressionAssign { $1 }
    | new word { ExpressionObjectNew $2 }
    | word { ExpressionVar $1 }

ExpressionAssign :: { Expression }
    : Expression "<-" Expression { ExpressionAssign $1 $3 }

ExpressionArithmetic :: { Expression }
    : Expression "++"   { ExpressionArithInc $1 }
    | Expression "--"   { ExpressionArithDec $1 }
    | Expression '+' Expression { ExpressionArithPlus $1 $3 }
    | Expression '-' Expression { ExpressionArithMinus $1 $3 }
    | Expression '*' Expression { ExpressionArithMul $1 $3 }
    | Expression '/' Expression { ExpressionArithDiv $1 $3 }
    | Expression '%' Expression { ExpressionArithMod $1 $3 }

ExpressionCompare :: { Expression }
    : Expression "==" Expression { ExpressionCompareEq $1 $3 }
    | Expression "!=" Expression { ExpressionCompareNeq $1 $3 }
    | Expression '<' Expression { ExpressionCompareLt  $1 $3 }
    | Expression "<=" Expression { ExpressionCompareLeq $1 $3 }
    | Expression '>' Expression { ExpressionCompareGt $1 $3 }
    | Expression ">=" Expression { ExpressionCompareGeq $1 $3 }

ExpressionLogic :: { Expression }
    : Expression "&&" Expression { ExpressionLogicAnd $1 $3 }
    | Expression "||" Expression { ExpressionLogicOr $1 $3 }
    | '!' Expression { ExpressionLogicNot $2 }

FunctionArguments :: { [Expression] }
    : {- empty -} { [] }
    | Farglist { $1 }

Farglist :: { [Expression] }
    : Expression { $1 : [] }
    | Farglist ',' Expression { $3 : $1 }

{

reportPos :: LexerPosition -> String
reportPos (LexPos _ l c) = "line "++ show l ++ ", column " ++ show c

parseError :: [LexToken] -> a
parseError ((PToken pos _):_) = error $ "parse Error at: "++ reportPos pos


}
