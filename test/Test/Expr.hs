module Test.Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators         (Parser (..), Result (..), runParser,
                                      symbol)
import           Control.Applicative ((<|>))
import           Control.Monad       ((>=>))
import           Data.Maybe          (maybe)
import           Expr
import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))

isFailure (Failure _) = True
isFailure  _          = False

unit_parseExpr :: Assertion
unit_parseExpr = do
    runParser parseExpr "1*2*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Num 2)) (Num 3))
    runParser parseExpr "123"     @?= Success "" (Num 123)
    runParser parseExpr "abc"     @?= Success "" (Ident "abc")
    runParser parseExpr "1*2+3*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)))
    runParser parseExpr "1+2*3+4" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Num 2) (Num 3))) (Num 4))
    runParser parseExpr "1*x*3"   @?= Success "" (BinOp Mult (BinOp Mult (Num 1) (Ident "x")) (Num 3))
    runParser parseExpr "xyz"     @?= Success "" (Ident "xyz")
    runParser parseExpr "1*x+z*4" @?= Success "" (BinOp Plus (BinOp Mult (Num 1) (Ident "x")) (BinOp Mult (Ident "z") (Num 4)))
    runParser parseExpr "1+y*3+z" @?= Success "" (BinOp Plus (BinOp Plus (Num 1) (BinOp Mult (Ident "y") (Num 3))) (Ident "z"))
    runParser parseExpr "1+x" @?= Success "" (BinOp Plus (Num 1) (Ident "x"))
    runParser parseExpr "1-x" @?= Success "" (BinOp Minus (Num 1) (Ident "x"))
    runParser parseExpr "1*x" @?= Success "" (BinOp Mult (Num 1) (Ident "x"))
    runParser parseExpr "1/x" @?= Success "" (BinOp Div (Num 1) (Ident "x"))
    runParser parseExpr "1^x" @?= Success "" (BinOp Pow (Num 1) (Ident "x"))
    runParser parseExpr "1==x" @?= Success "" (BinOp Equal (Num 1) (Ident "x"))
    runParser parseExpr "1/=x" @?= Success "" (BinOp Nequal (Num 1) (Ident "x"))
    runParser parseExpr "1>x" @?= Success "" (BinOp Gt (Num 1) (Ident "x"))
    runParser parseExpr "1>=x" @?= Success "" (BinOp Ge (Num 1) (Ident "x"))
    runParser parseExpr "1<x" @?= Success "" (BinOp Lt (Num 1) (Ident "x"))
    runParser parseExpr "1<=x" @?= Success "" (BinOp Le (Num 1) (Ident "x"))
    runParser parseExpr "1&&x" @?= Success "" (BinOp And (Num 1) (Ident "x"))
    runParser parseExpr "1||x" @?= Success "" (BinOp Or (Num 1) (Ident "x"))
    runParser parseExpr "(1==x+2)||3*4<y-5/6&&(7/=z^8)||(id>12)&&abc<=13||xyz>=42" @?=
      runParser parseExpr "(1==(x+2))||(((3*4)<(y-(5/6))&&(7/=(z^8)))||(((id>12)&&(abc<=13))||(xyz>=42)))"

unit_unaryEpxr = do
    runParser parseExpr "-1+2" @?= Success "" (BinOp Plus (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1*2" @?= Success "" (BinOp Mult (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1==2" @?= Success "" (BinOp Equal (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1==-2" @?= Success "" (BinOp Equal (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    runParser parseExpr "-1&&-2" @?= Success "" (BinOp And (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    runParser parseExpr "!1&&!2" @?= Success "" (BinOp And (UnaryOp Not (Num 1)) (UnaryOp Not (Num 2)))
    runParser parseExpr "-1^2" @?= Success "" (UnaryOp Minus (BinOp Pow (Num 1) (Num 2)))
    runParser parseExpr "-1^(-2)" @?= Success "" (UnaryOp Minus (BinOp Pow (Num 1) (UnaryOp Minus (Num 2))))
    runParser parseExpr "(-1)^2" @?= Success "" (BinOp Pow (UnaryOp Minus (Num 1)) (Num 2))
    runParser parseExpr "-1+-2" @?= Success "" (BinOp Plus (UnaryOp Minus (Num 1)) (UnaryOp Minus (Num 2)))
    runParser parseExpr "!-1" @?= Success "" (UnaryOp Not (UnaryOp Minus (Num 1)))
    runParser parseExpr "!(-1)" @?= Success "" (UnaryOp Not (UnaryOp Minus (Num 1)))
    runParser parseExpr "-(!1)" @?= Success "" (UnaryOp Minus (UnaryOp Not (Num 1)))
    runParser parseExpr "-1---2" @?= Success ""
      (BinOp Minus (UnaryOp Minus (Num 1)) (UnaryOp Minus $ UnaryOp Minus $ Num 2))
    assertBool "" $ isFailure $ runParser parseExpr "-1^-2"
    assertBool "" $ isFailure $ runParser parseExpr "-!1"
