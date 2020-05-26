{
module ParseP (parse) where

import qualified LexP as L
import qualified PLang as P

import Data.List (groupBy)
import Data.Function (on)
}

%name parse
%tokentype { L.Token }
%error { parseError }

%token
      
      VAR   { L.Var $$ }
      IDENT { L.Ident $$ }
      COMMA { L.Comma }
      STOP  { L.Stop }
      LPAR  { L.LPar }
      RPAR  { L.RPar }
      ARR   { L.Arr }
      GOAL  { L.Goal }

%%

program :: { P.Program }
        : list0(ruleF, eps) GOAL body { P.Program (processRules $1) $3 }

eps
    : {- empty -} { () }

revList1(p, s)
    : p { [$1] }
    | revList1(p, s) s p { $3 : $1 }

revList0(p, s)
    : {- empty -} { [] }
    | revList1(p, s) { $1 }

list1(p, s)
    : revList1(p, s) { reverse $1 }

list0(p, s)
    : revList0(p, s) { reverse $1 }

ruleF :: { RuleFull }
      : atom body { RuleFull $1 $2 }

body :: { P.Body }
     : STOP { [] }
     | ARR list1(atom, COMMA) STOP { $2 }

args :: { P.Args }
     : {- empty -} { [] }
     | LPAR list1(arg, COMMA) RPAR { $2 }

arg :: { P.Arg }
    : VAR { P.VarArg $1 }
    | atom { P.AtomArg $1 }

atom :: { P.Atom }
     : IDENT args { P.Atom $1 $2 }

{
data RuleFull = RuleFull P.Atom P.Body

getName (RuleFull (P.Atom name _) _) = name

mkRelation rules = P.Relation name (map mkRule rules)
  where
    name = getName (head rules)
    mkRule (RuleFull (P.Atom _ args) body) = P.Rule args body

processRules = map mkRelation . groupBy ((==) `on` getName)

parseError = error "Parse error"
}
