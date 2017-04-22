{
module Parser.Parser (parse) where
import Ast.Expression
import Ast.Type
import Ast.ExprTree
import Parser.Tokens
import qualified Data.Map as Map
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
    "Void"    { ( _ , TokenVoid ) }
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


Program :: { ExprTree ParserExpression }
    : ExpressionSequence { ExprTree (UnknownType, ExpressionBlock, (0,0,0)) $1 }
    ;

ExpressionSequence :: { [ExprTree ParserExpression] }
    : OtherExpression ';'    { [$1] }
    | ExpressionSequence OtherExpression ';' { $1 ++ [$2] }
    | OptionalSemicolonExpression ExpressionTerminator { [$1] }
    | ExpressionSequence OptionalSemicolonExpression ExpressionTerminator { $1 ++ [$2] }
    ;

ExpressionTerminator :: { () }
    : ';'   { () }
    | error { () }
    ;

ExpressionList :: { [ExprTree ParserExpression] }
    : Expression { [$1] }
    | ExpressionList ',' Expression { $1 ++ [$3] }
    ;

Expression :: { ExprTree ParserExpression }
    : OptionalSemicolonExpression { $1 }
    | OtherExpression { $1 }
    ;

OptionalSemicolonExpression :: { ExprTree ParserExpression }
    : '{' ExpressionSequence '}' { ExprTree (UnknownType,ExpressionBlock,fst $1) $2 }
    | ExpressionIf { $1 }
    | ExpressionLoop { $1 }
    ;

FunctionArgs :: { [ExprTree ParserExpression] }
    : {- empty -} { [] }
    | ExpressionList { $1 }
    ;

OtherExpression :: { ExprTree ParserExpression }
    : '(' Expression ')' { $2 }
    | Expression '(' FunctionArgs ')'
        { ExprTree (UnknownType,ExpressionFunctionCall,getExpPos $1) ($1:$3) }
    | pure Expression { ExprTree (UnknownType,ExpressionPure,fst $1) [$2] }
    | tainted Expression { ExprTree (UnknownType,ExpressionPure,fst $1) [$2] }
    | load Dotpath { liftExpression (ExpressionLoad (init $2) (last $2)) (fst $1) }
    | return Expression { ExprTree (UnknownType,ExpressionReturn,fst $1) [$2] }
    | new word { liftExpression (ExpressionNew $2) (fst $1) }
    | ExpressionArith { $1 }
    | FieldAccessExpression { $1 }
    | ExpressionAssign { $1 }
    | ExpressionDeclaration { $1 }
    | ExpressionConstant { $1 }
    | ExpressionComp { $1 }
    ;

Condition :: { ExprTree ParserExpression }
    : '(' Expression ')' { $2 }
    ;

ExpressionIf :: { ExprTree ParserExpression }
    : if Condition Expression else Expression
        { ExprTree (UnknownType,ExpressionIf,fst $1) [$2,$3,$5] }
    | if Condition Expression { ExprTree (UnknownType,ExpressionIf,fst $1) [$2,$3] }
    ;

ExpressionLoop :: { ExprTree ParserExpression }
    : while Condition Expression { ExprTree (UnknownType,ExpressionIf,fst $1) [$2,$3] }

ExpressionArith :: { ExprTree ParserExpression }
    : Expression '*' Expression  { ExprTree (UnknownType,ExpressionArithMul,fst $2) [$1,$3] }
    | Expression '/' Expression  { ExprTree (UnknownType,ExpressionArithDiv,fst $2) [$1,$3] }
    | Expression '+' Expression  { ExprTree (UnknownType,ExpressionArithPlus,fst $2) [$1,$3] }
    | Expression '-' Expression  { ExprTree (UnknownType,ExpressionArithMinus,fst $2) [$1,$3] }
    | Expression '%' Expression  { ExprTree (UnknownType,ExpressionArithMod,fst $2) [$1,$3] }
    | ExpressionLookup "++"      { ExprTree (UnknownType,ExpressionInc,fst $2) [$1] }
    | ExpressionLookup "--"      { ExprTree (UnknownType,ExpressionDec,fst $2) [$1] }
    | '!' Expression             { ExprTree (UnknownType,ExpressionNot,fst $1) [$2] }
    | Expression "&&" Expression { ExprTree (UnknownType,ExpressionAnd,fst $2) [$1,$3] }
    | Expression "||" Expression { ExprTree (UnknownType,ExpressionOr,fst $2) [$1,$3] }
    ;

ExpressionComp :: { ExprTree ParserExpression }
    : Expression "==" Expression { ExprTree (UnknownType,ExpressionEq,fst $2) [$1,$3] }
    | Expression "!=" Expression { ExprTree (UnknownType,ExpressionNeq,fst $2) [$1,$3] }
    | Expression '<' Expression  { ExprTree (UnknownType,ExpressionLt,fst $2) [$1,$3] }
    | Expression "<=" Expression { ExprTree (UnknownType,ExpressionLeq,fst $2) [$1,$3] }
    | Expression '>' Expression  { ExprTree (UnknownType,ExpressionGt,fst $2) [$1,$3] }
    | Expression ">=" Expression { ExprTree (UnknownType,ExpressionGeq,fst $2) [$1,$3] }
    ;

ExpressionConstant :: { ExprTree ParserExpression }
    : int       { setExpPos (fst tok) (astConstant TypeInt $ ConstantInt $1) }
    | stringlit { setExpPos (fst tok) (astConstant TypeString $ ConstantString $1) }
    | true      { setExpPos (fst $1) (astConstant TypeBool $ ConstantBool True) }
    | false     { setExpPos (fst $1) (astConstant TypeBool $ ConstantBool False) }
    | float     { setExpPos (fst tok) (astConstant TypeFloat $ ConstantFloat $1) }
    ;

FieldAccessExpression :: { ExprTree ParserExpression }
    : ExpressionLookup { $1 }
    | ExpressionArrayAccess { $1 }
    | ExpressionMemberAccess { $1 }
    ;

ExpressionMemberAccess :: { ExprTree ParserExpression }
    : Expression '.' word { ExprTree (UnknownType,ExpressionMemberAccess $3, fst $2) [$1] }
    ;

ExpressionArrayAccess :: { ExprTree ParserExpression }
    : Expression '[' Expression ']' { ExprTree (UnknownType,ExpressionArrayAccess,fst $2) [$1,$3] }

ExpressionAssign :: { ExprTree ParserExpression }
    : FieldAccessExpression '=' Expression
        { ExprTree (UnknownType,ExpressionAssign,fst $2) [$1,$3] }
    | ExpressionVarDeclaration '=' Expression
        { (\(ExprTree p s) e -> ExprTree p $ s ++ [e]) $1 $3 }

ExpressionLookup :: { ExprTree ParserExpression }
    : word { liftExpression (ExpressionLookup $1) (fst tok)}
    ;

TypeDeclaration :: { ExprType }
    : "String" { TypeString }
    | "Int" { TypeInt }
    | "Float" { TypeFloat }
    | "Bool" { TypeBool }
    | "Void" { TypeVoid }
    | '[' TypeDeclaration ']' { TypeArray $2 }
    | '{' DeclaratorList '}' { TypeTypedef . decllistToMap $ $2 }
    | '(' ArrowSeparatedList ')' { TypeFunction $2 }
    | word { TypeUnresolved $1 }
    ;

ArrowSeparatedList :: { [ExprType] }
    : TypeDeclaration { [$1] }
    | ArrowSeparatedList "->" TypeDeclaration { $1 ++ [$3] }
    ;

Declarator :: { Declarator }
    : word { Declarator $1 UnknownType }
    | word ':' TypeDeclaration { Declarator $1 $3 }
    ;

DeclaratorList :: { [Declarator] }
    : {- empty -} { [] }
    | Declarator { [$1] }
    | DeclaratorList ',' Declarator { $1 ++ [$3] }
    ;

ExpressionVarDeclaration :: { ExprTree ParserExpression }
    : var Declarator
        { ExprTree (TypeVoid,ExpressionVarDeclaration $2,fst $1) [] }
    ;

FunctionTypeDeclarator :: { ExprType }
    : {-- empty --} { UnknownType }
    | ':' TypeDeclaration { $2 }
    ;

ExpressionFunctionDeclaration :: { ExprTree ParserExpression }
    : function word '(' DeclaratorList ')' FunctionTypeDeclarator Expression
        { ExprTree (TypeFunction $ decllistToExptypelist $4,
            ExpressionNamedFunctionDeclaration $2 $4 $6,
            fst $1) [$7]}
    | function '(' DeclaratorList ')' FunctionTypeDeclarator Expression
        { ExprTree (TypeFunction $ decllistToExptypelist $3,
            ExpressionAnonFunctionDeclaration $3 $5,
            fst $1) [$6] }
    | '\\' DeclaratorList "->" Expression %prec BACKSLASHDECL
        { ExprTree (TypeFunction $ decllistToExptypelist $2,
            ExpressionAnonFunctionDeclaration $2 UnknownType,
            fst $1) [$4] }
    ;

ExpressionDeclaration :: { ExprTree ParserExpression }
    : ExpressionVarDeclaration { $1 }
    | ExpressionFunctionDeclaration { $1 }
    | typedef word '{' DeclarationList '}'
        { ExprTree (TypeVoid,ExpressionTypedefDeclaration $2, fst $1) $4 }
    ;

DeclarationList :: { [ExprTree ParserExpression] }
    : ExpressionDeclaration ';' { [$1] }
    | DeclarationList ExpressionDeclaration ';' { $1 ++ [$2] }
    ;

Dotpath :: { [String] }
    : word { [$1] }
    | '.' '.' { [".."] }
    | Dotpath '.' '.' '.' { $1 ++ [".."] }
    | Dotpath '.' word { $1 ++ [$3] }
    ;
{

liftExpression :: ExpressionBase -> LexerPosition -> ExprTree ParserExpression
liftExpression a b = ExprTree (UnknownType,a,b) []

reportPos :: LexerPosition -> String
reportPos (_, l, c) = "line "++ show l ++ ", column " ++ show c

reportError :: Token -> String
reportError a = "unexpected token: "++show a

parseError :: [LexToken] -> a
parseError [] = error "parse error: unexpected eof, did you forget a closing brace or semicolon?"
parseError ((pos,tok):_) = error $ "parse Error at: "++ reportPos pos ++ "\n"
    ++ reportError tok

setExpPos :: LexerPosition -> ExprTree ParserExpression -> ExprTree ParserExpression
setExpPos p (ExprTree (t,b,_) s) = ExprTree (t,b,p) s

getExpPos :: ExprTree ParserExpression -> LexerPosition
getExpPos (ExprTree (_,_,p) _) = p

astConstant :: ExprType -> Constant -> ExprTree ParserExpression
astConstant t c = ExprTree (t,ExpressionConstant c,(-1,-1,-1)) []

decllistToMap :: [Declarator] -> Map.Map String ExprType
decllistToMap a = Map.fromList $ map (\(Declarator a b) -> (a,b)) a

decllistToExptypelist :: [Declarator] -> [ExprType]
decllistToExptypelist = map (\ (Declarator _ t) -> t)

}
