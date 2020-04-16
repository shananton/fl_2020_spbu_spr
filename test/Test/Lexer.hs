module Test.Lexer where

import           AST
import           Combinators         (Parser, Result (..), parse,
                                      runParser, satisfy, sepBy1, symbol, toStream)
import           Lexer

import           Test.Tasty.HUnit    (Assertion, assertBool, (@?=))

import           Control.Applicative (many, some)

isFailure (Failure _) = True
isFailure  _          = False

unit_nat :: Assertion
unit_nat = do
    runParser lexAll "876" @?= Success (toStream "" 3) [TInt 876, TSep Newline]
    runParser lexAll "00000876" @?= Success (toStream "" 8) [TInt 876, TSep Newline]
    runParser lexAll "0x876" @?= Success (toStream "" 5) [TInt 0x876, TSep Newline]

unit_operators :: Assertion
unit_operators = do
    runParser lexAll ">=" @?= Success (toStream "" 2) [TOperator (Arith Ge), TSep Newline]
    runParser lexAll ">==" @?= Success (toStream "" 3) [TOperator (Arith Ge), TOperator Assign, TSep Newline]

unit_indent :: Assertion
unit_indent = do
    runParser lexAll "0\n 1\n  2\n 1\n" @?= Success (toStream "" 12)
        [ TInt 0, TSep Newline
        , TSep Indent, TInt 1, TSep Newline
        , TSep Indent, TInt 2, TSep Newline
        , TSep Dedent, TInt 1, TSep Newline
        , TSep Dedent
        ]
    runParser lexAll "0\n  2\n 1\n" @?= Success (toStream " 1\n" 6)
        [ TInt 0, TSep Newline
        , TSep Indent, TInt 2, TSep Newline
        , TSep Dedent
        ]

unit_lineCont :: Assertion
unit_lineCont = do
    runParser lexAll "line1    \\  \n   line2" @?= Success (toStream "" 21)
        [TId "line1", TId "line2", TSep Newline]
