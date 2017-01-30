{
module Lexer (lexer) where

import Tokens
}

%wrapper "posn"

$digit = 0-9			-- digits
$alpha = [a-zA-Z]		-- alphabetic characters

tokens :-

  $white+			  	;
  if				  	  { \p s -> PToken (toPos p) TokenIf }
  then					  { \p s -> PToken (toPos p) TokenThen }
  else 					  { \p s -> PToken (toPos p) TokenElse }
  fi			   		  { \p s -> PToken (toPos p) TokenFi }
  while 			    { \p s -> PToken (toPos p) TokenWhile }
  do 				   	  { \p s -> PToken (toPos p) TokenDo }
  od				  	  { \p s -> PToken (toPos p) TokenOd }
  repeat          { \p s -> PToken (toPos p) TokenRepeat }
  until           { \p s -> PToken (toPos p) TokenUntil }
  for             { \p s -> PToken (toPos p) TokenFor }
  to              { \p s -> PToken (toPos p) TokenTo }
  downto          { \p s -> PToken (toPos p) TokenDownto }
  return          { \p s -> PToken (toPos p) TokenReturn }
  function        { \p s -> PToken (toPos p) TokenFunction }
  "class"         { \p s -> PToken (toPos p) TokenClass}
  new             { \p s -> PToken (toPos p) TokenNew }
  \;				  	  { \p s -> PToken (toPos p) TokenSemicolon }
  \(    		      { \p s -> PToken (toPos p) TokenRBOpen }
  \)				  	  { \p s -> PToken (toPos p) TokenRBClose }
  \{				  	  { \p s -> PToken (toPos p) TokenCBOpen }
  \}				  	  { \p s -> PToken (toPos p) TokenCBClose }
  \[              { \p s -> PToken (toPos p) TokenSBOpen }
  \]              { \p s -> PToken (toPos p) TokenSBClose }
  "<-"					  { \p s -> PToken (toPos p) TokenLeftarrow }
  ==              { \p s -> PToken (toPos p) TokenCompEq }
  "!="            { \p s -> PToken (toPos p) TokenCompNeq }
  ">="            { \p s -> PToken (toPos p) TokenCompGeq }
  "<="            { \p s -> PToken (toPos p) TokenCompLeq }
  "&&"            { \p s -> PToken (toPos p) TokenLogicAnd }
  "||"            { \p s -> PToken (toPos p) TokenLogicOr }
  \>              { \p s -> PToken (toPos p) TokenCompGt }
  \<              { \p s -> PToken (toPos p) TokenCompLt }
  \+              { \p s -> PToken (toPos p) TokenArithPlus }
  \-              { \p s -> PToken (toPos p) TokenArithMinus }
  \*              { \p s -> PToken (toPos p) TokenArithMul }
  \/              { \p s -> PToken (toPos p) TokenArithDiv }
  \%              { \p s -> PToken (toPos p) TokenArithMod }
  "++"            { \p s -> PToken (toPos p) TokenArithInc }
  "--"            { \p s -> PToken (toPos p) TokenArithDec }
  \,              { \p s -> PToken (toPos p) TokenComma }
  \.              { \p s -> PToken (toPos p) TokenDot }
  \".+\"          { \p s -> PToken (toPos p) $ TokenStringLit (reverse . tail . reverse . tail $ s) }
  '!'             { \p s -> PToken (toPos p) TokenLogicNot }
  $digit+         { \p s -> PToken (toPos p) $ TokenInt (read s) }
  $alpha+ 				{ \p s -> PToken (toPos p) $ TokenWord s}

{
-- Each action has type :: AlexPosn String -> LexToken

toPos :: AlexPosn -> LexerPosition
toPos (AlexPn off line column ) = LexPos off line column

lexer = alexScanTokens
}
