{
module LexP (Token (..), tokenize) where
}

%wrapper "basic"

$uppercase = [A-Z]
$lowercase = [a-z]
$digit = [0-9]
$alnum = [$uppercase $lowercase $digit]
$space = [\ \t\n\r]

tokens :-
  
  $space+               ;
  $uppercase $alnum*    { \s -> Var s }
  $lowercase $alnum*    { \s -> Ident s }
  \,                    { \_ -> Comma }
  \.                    { \_ -> Stop }
  \(                    { \_ -> LPar }
  \)                    { \_ -> RPar }
  ":-"                  { \_ -> Arr }
  "?-"                  { \_ -> Goal }


{
tokenize = alexScanTokens

data Token = Var String
           | Ident String
           | Comma
           | Stop
           | LPar
           | RPar
           | Arr
           | Goal
  deriving (Eq, Show)
}
