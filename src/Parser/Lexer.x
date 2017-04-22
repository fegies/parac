{
module Parser.Lexer (lexer) where

import Parser.Tokens
import Helpers.HexMod
}

%wrapper "posn"

$digit = 0-9      -- digits
$alpha = [a-zA-Z]   -- alphabetic characters

tokens :-

  $white+                   ;
  \/\/.*                    ;
  \/\*[.\n]*\*\/            ;
  if                        { \p s -> ( p , TokenIf ) }
  else                      { \p s -> ( p , TokenElse ) }
  while                     { \p s -> ( p , TokenWhile ) }
  return                    { \p s -> ( p , TokenReturn ) }
  true                      { \p s -> ( p , TokenTrue ) }
  false                     { \p s -> ( p , TokenFalse ) }
  extends                   { \p s -> ( p , TokenExtends ) }
  var                       { \p s -> ( p , TokenVar) }
  function                  { \p s -> ( p , TokenFunction ) }
  "typedef"                 { \p s -> ( p , TokenTypedef ) }
  new                       { \p s -> ( p , TokenNew ) }
  String                    { \p s -> ( p , TokenStringType ) }
  Int                       { \p s -> ( p , TokenIntType ) }
  Bool                      { \p s -> ( p , TokenBoolType ) }
  Float                     { \p s -> ( p , TokenFloatType ) }
  Void                      { \p s -> ( p , TokenVoid ) }
  import                    { \p s -> ( p , TokenLoad ) }
  \\                        { \p s -> ( p , TokenBackslash ) }
  \;                        { \p s -> ( p , TokenSemicolon ) }
  \(                        { \p s -> ( p , TokenRBOpen ) }
  \)                        { \p s -> ( p , TokenRBClose ) }
  \{                        { \p s -> ( p , TokenCBOpen ) }
  \}                        { \p s -> ( p , TokenCBClose ) }
  \[                        { \p s -> ( p , TokenSBOpen ) }
  \]                        { \p s -> ( p , TokenSBClose ) }
  =                         { \p s -> ( p , TokenAssign ) }
  ==                        { \p s -> ( p , TokenCompEq ) }
  "!="                      { \p s -> ( p , TokenCompNeq ) }
  ">="                      { \p s -> ( p , TokenCompGeq ) }
  "<="                      { \p s -> ( p , TokenCompLeq ) }
  "&&"                      { \p s -> ( p , TokenLogicAnd ) }
  "||"                      { \p s -> ( p , TokenLogicOr ) }
  "->"                      { \p s -> ( p , TokenRightarrow ) }
  \>                        { \p s -> ( p , TokenCompGt ) }
  \<                        { \p s -> ( p , TokenCompLt ) }
  \+                        { \p s -> ( p , TokenArithPlus ) }
  \-                        { \p s -> ( p , TokenArithMinus ) }
  \*                        { \p s -> ( p , TokenArithMul ) }
  \/                        { \p s -> ( p , TokenArithDiv ) }
  \%                        { \p s -> ( p , TokenArithMod ) }
  "++"                      { \p s -> ( p , TokenArithInc ) }
  "--"                      { \p s -> ( p , TokenArithDec ) }
  \,                        { \p s -> ( p , TokenComma ) }
  \.                        { \p s -> ( p , TokenDot ) }
  \:                        { \p s -> ( p , TokenColon ) }
  \".+\"                    { \p s -> ( p , TokenStringLit $ init . tail $ s ) }
  \!                        { \p s -> ( p , TokenLogicNot ) }
  pure                      { \p s -> ( p , TokenPure ) }
  tainted                   { \p s -> ( p , TokenTainted ) }
  [$digit]+ \. [$digit]+    { \p s -> ( p , TokenFloat $ read s) }
  [$digit]+ f               { \p s -> ( p , TokenFloat . read . init $ s) }
  0x[0-9a-fA-F]+            { \p s -> ( p , TokenInt . readHex . drop 2 $ s) }
  $digit+                   { \p s -> ( p , TokenInt $ read s ) }
  [$alpha][$alpha$digit\_]* { \p s -> ( p , TokenWord s ) }

{
-- Each action has type :: AlexPosn String -> LexToken

toPos :: AlexPosn -> LexerPosition
toPos (AlexPn off line column ) = (off, line, column)

lexer = (map (\(p, t) -> (toPos p,t) )) . alexScanTokens
}
