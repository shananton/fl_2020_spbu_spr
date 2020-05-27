module Test.LexP where

import Test.Tasty.HUnit (Assertion, (@?=))
import Common (assertFails)

import LexP

unit_simpleTokensAndSpacing :: Assertion
unit_simpleTokensAndSpacing = do
  let tokens = [Ident "id", Var "Var", Goal, Arr, Stop, Comma, LPar, RPar]
  tokenize "id Var ?- :- . , ( )" @?= tokens
  tokenize "id Var?-:-.,()" @?= tokens
  tokenize "id\r\n \t Var ?- \n:-\n., (\t\t)" @?= tokens
  assertFails $ tokenize "? -"
  assertFails $ tokenize ": -"

unit_identifiers :: Assertion
unit_identifiers = do
  let lowercase = "lowercase084615LuL"
  let uppercase = "Uppercase12856Zasho"
  tokenize lowercase @?= [Ident lowercase]
  tokenize uppercase @?= [Var uppercase]
