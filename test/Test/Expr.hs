module Test.Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol)
import           Control.Applicative ((<|>))
import           Control.Monad       ((>=>))
import           Data.Maybe          (maybe)
import           Expr
import           Lexer
import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))

expr' = expr <* symbol (TSep Newline)

isFailure (Left _) = True
isFailure  _       = False

unit_parseExpr :: Assertion
unit_parseExpr = do
    parseRawEither expr' "1*2*3"   @?= Right (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    parseRawEither expr' "123"     @?= Right (Num 123)
    parseRawEither expr' "abc"     @?= Right (Ident "abc")
    parseRawEither expr' "1*2+3*4" @?= Right (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    parseRawEither expr' "1+2*3+4" @?= Right (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4))
    parseRawEither expr' "1*x*3"   @?= Right (BinOp Mult (BinOp Mult (Num 1) (Ident "x")) (Num 3))
    parseRawEither expr' "xyz"     @?= Right (Ident "xyz")
    parseRawEither expr' "1*x+z*4" @?= Right (BinOp Plus (BinOp Mult (Num 1) (Ident "x")) (BinOp Mult (Ident "z") (Num 4)))
    parseRawEither expr' "1+y*3+z" @?= Right (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3))) (Ident "z"))
    parseRawEither expr' "1+x" @?= Right (BinOp Plus (Num 1) (Ident "x"))
    parseRawEither expr' "1-x" @?= Right (BinOp Minus (Num 1) (Ident "x"))
    parseRawEither expr' "1*x" @?= Right (BinOp Mult (Num 1) (Ident "x"))
    parseRawEither expr' "1/x" @?= Right (BinOp Div (Num 1) (Ident "x"))
    parseRawEither expr' "1^x" @?= Right (BinOp Pow (Num 1) (Ident "x"))
    parseRawEither expr' "1==x" @?= Right (BinOp Equal (Num 1) (Ident "x"))
    parseRawEither expr' "1/=x" @?= Right (BinOp Nequal (Num 1) (Ident "x"))
    parseRawEither expr' "1>x" @?= Right (BinOp Gt (Num 1) (Ident "x"))
    parseRawEither expr' "1>=x" @?= Right (BinOp Ge (Num 1) (Ident "x"))
    parseRawEither expr' "1<x" @?= Right (BinOp Lt (Num 1) (Ident "x"))
    parseRawEither expr' "1<=x" @?= Right (BinOp Le (Num 1) (Ident "x"))
    parseRawEither expr' "1&&x" @?= Right (BinOp And (Num 1) (Ident "x"))
    parseRawEither expr' "1||x" @?= Right (BinOp Or (Num 1) (Ident "x"))
    parseRawEither expr' "(1==x+2)||3*4<y-5/6&&(7/=z^8)||(id>12)&&abc<=13||xyz>=42" @?=
      parseRawEither expr' "(1==(x+2))||(((3*4)<(y-(5/6))&&(7/=(z^8)))||(((id>12)&&(abc<=13))||(xyz>=42)))"

unit_unaryEpxr = do
    parseRawEither expr' "-1+2" @?= Right (BinOp Plus (UnaryOp Minus (Num 1)) (Num 2))
    parseRawEither expr' "-1*2" @?= Right (BinOp Mult (UnaryOp Minus (Num 1)) (Num 2))
    parseRawEither expr' "-1==2" @?= Right (BinOp Equal (UnaryOp Minus (Num 1)) (Num 2))
    parseRawEither expr' "-1==-2" @?= Right (BinOp Equal (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    parseRawEither expr' "-1&&-2" @?= Right (BinOp And (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    parseRawEither expr' "!1&&!2" @?= Right (BinOp And (UnaryOp Not (Num 1)) (UnaryOp Not (Num 2)))
    parseRawEither expr' "-1^2" @?= Right (UnaryOp Minus (BinOp Pow (Num 1) (Num 2)))
    parseRawEither expr' "-1^(-2)" @?= Right (UnaryOp Minus (BinOp Pow (Num 1) (UnaryOp Minus (Num 2))))
    parseRawEither expr' "(-1)^2" @?= Right (BinOp Pow (UnaryOp Minus (Num 1)) (Num 2))
    parseRawEither expr' "-1+-2" @?= Right (BinOp Plus (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    parseRawEither expr' "!-1" @?= Right (UnaryOp Not (UnaryOp Minus (Num 1)))
    parseRawEither expr' "!(-1)" @?= Right (UnaryOp Not (UnaryOp Minus (Num 1)))
    parseRawEither expr' "-(!1)" @?= Right (UnaryOp Minus (UnaryOp Not (Num 1)))
    parseRawEither expr' "-1---2" @?= Right
      (BinOp Minus (UnaryOp Minus (Num 1)) (UnaryOp Minus $ UnaryOp Minus $ Num 2))
    assertBool "" $ isFailure $ parseRawEither expr' "-1^-2"
    assertBool "" $ isFailure $ parseRawEither expr' "-!1"
