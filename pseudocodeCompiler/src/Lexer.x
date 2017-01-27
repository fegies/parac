{
module Lexer (lexer) where

import Tokens
}

%wrapper "basic"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+			  	;
  if				  	  { \s -> TokenIf }
  then					  { \s -> TokenThen }
  else 					  { \s -> TokenElse }
  fi			   		  { \s -> TokenFi }
  while 			    { \s -> TokenWhile }
  do 				   	  { \s -> TokenDo }
  od				  	  { \s -> TokenOd }
  repeat          { \s -> TokenRepeat }
  until           { \s -> TokenUntil }
  for             { \s -> TokenFor }
  to              { \s -> TokenTo }
  downto          { \s -> TokenDownto }
  return          { \s -> TokenReturn }
  function        { \s -> TokenFunction }
  "class"         { \s -> TokenClass}
  new             { \s -> TokenNew }
  \;				  	  { \s -> TokenSemicolon }
  \(    		      { \s -> TokenRBOpen }
  \)				  	  { \s -> TokenRBClose }
  \{				  	  { \s -> TokenCBOpen }
  \}				  	  { \s -> TokenCBClose }
  \[              { \s -> TokenSBOpen }
  \]              { \s -> TokenSBClose }
  "<-"					  { \s -> TokenLeftarrow }
  ==              { \s -> TokenCompEq }
  "!="            { \s -> TokenCompNeq }
  ">="            { \s -> TokenCompGeq }
  "<="            { \s -> TokenCompLeq }
  "&&"            { \s -> TokenLogicAnd }
  "||"            { \s -> TokenLogicOr }
  \>              { \s -> TokenCompGt }
  \<              { \s -> TokenCompLt }
  \+              { \s -> TokenArithPlus }
  \-              { \s -> TokenArithMinus }
  \*              { \s -> TokenArithMul }
  \/              { \s -> TokenArithDiv }
  \%              { \s -> TokenArithMod }
  "++"            { \s -> TokenArithInc }
  "--"            { \s -> TokenArithDec }
  \,              { \s -> TokenComma }
  \.              { \s -> TokenDot }
  \".+\"      { \s -> TokenStringLit (reverse . tail . reverse . tail $ s) }
  '!'             { \s -> TokenLogicNot }
  $digit+         { \s -> TokenInt (read s) }
  $alpha+ 				{ \s -> TokenWord s}

{
-- Each action has type :: String -> Token

lexer = alexScanTokens
}
