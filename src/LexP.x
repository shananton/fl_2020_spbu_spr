{
module LexP (Token (..), alexScanTokens) where
}

%wrapper "basic"

$uppercase = [A-Z]
$lowercase = [a-z]
$digit = [0-9]
$alnum = [$uppercase $lowercase $digit]
$space = [\ \n\r]

tokens :-
  
  $space+               ;
  $uppercase $alnum*    { \s -> TVar s }
  $lowercase $alnum*    { \s -> TIdent s }
  \,                    { \_ -> TComma }
  \.                    { \_ -> TStop }
  \(                    { \_ -> TLPar }
  \)                    { \_ -> TRPar }
  ":-"                  { \_ -> TArr }
  "?-"                  { \_ -> TGoal }


{
data Token = TVar String
           | TIdent String
           | TComma
           | TStop
           | TLPar
           | TRPar
           | TArr
           | TGoal
}
