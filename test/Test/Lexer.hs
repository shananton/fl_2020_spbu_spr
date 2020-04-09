module Test.Lexer where

import           AST
import           Combinators         (Parser, Result (..), elem', parse,
                                      runParser, satisfy, sepBy1, symbol)
import           Lexer

import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))

import           Control.Applicative (many, some)

isFailure (Failure _) = True
isFailure  _          = False

unit_nat :: Assertion
unit_nat = do
    runParser lexAll "876" @?= Success "" [TInt 876, TSep Newline]
    runParser lexAll "00000876" @?= Success "" [TInt 876, TSep Newline]
    runParser lexAll "0x876" @?= Success "" [TInt 0x876, TSep Newline]

unit_operators :: Assertion
unit_operators = do
    runParser lexAll ">=" @?= Success "" [TOperator (Arith Ge), TSep Newline]
    runParser lexAll ">==" @?= Success "" [TOperator (Arith Ge), TOperator Assign, TSep Newline]

unit_indent :: Assertion
unit_indent = do
    runParser lexAll "0\n 1\n  2\n 1\n" @?= Success ""
        [ TInt 0, TSep Newline
        , TSep Indent, TInt 1, TSep Newline
        , TSep Indent, TInt 2, TSep Newline
        , TSep Dedent, TInt 1, TSep Newline
        , TSep Dedent
        ]
    runParser lexAll "0\n  2\n 1\n" @?= Success " 1\n"
        [ TInt 0, TSep Newline
        , TSep Indent, TInt 2, TSep Newline
        , TSep Dedent
        ]

unit_lineCont :: Assertion
unit_lineCont = do
    runParser lexAll "line1    \\  \n   line2" @?= Success ""
        [TId "line1", TId "line2", TSep Newline]
