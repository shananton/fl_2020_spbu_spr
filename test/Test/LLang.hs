module Test.LLang where

import Combinators         (Parser, Result (..), elem', runParser,
                                   satisfy, sepBy1, symbol, parse)
import LLang
import AST
import qualified Expr as E

import Test.Tasty.HUnit    (Assertion, (@?=), assertBool)

import Control.Applicative (some, many)

isFailure (Failure _) = True
isFailure  _          = False

unit_basic :: Assertion
unit_basic = do
    runParser parseL "x=1" @?= Success "" (Assign "x" (Num 1))
    runParser parseL "x  =   2  " @?= Success "" (Assign "x" (Num 2))
    assertBool "" $ isFailure $ runParser parseL " x=2"

    runParser parseL "read var_name" @?= Success "" (Read "var_name")
    assertBool "" $ isFailure $ runParser parseL "read read"
    runParser parseL "read read_val" @?= Success "" (Read "read_val")
    runParser parseL "write 1" @?= Success "" (Write (Num 1))


unit_lexpr :: Assertion
unit_lexpr = do
    runParser parseL "write 1+2" @?= Success "" (Write (BinOp Plus (Num 1) (Num 2)))
    runParser parseL "write 1+-2" @?= Success "" (Write (BinOp Plus (Num 1) (UnaryOp Minus (Num 2))))
    runParser parseL "write --1" @?= Success "" (Write (UnaryOp Minus $ UnaryOp Minus $ Num 1))
    runParser parseL "write !flag" @?= Success "" (Write $ UnaryOp Not $ Ident "flag")

    longExpr <- parse E.parseExpr "(1==x+2)||3*4<y-5/6&&(7/=z^8)||(id>12)&&abc<=13||xyz>=42"
    runParser parseL "write (1==x+2)||3*4<y-5/6&&(7/=z^8)||(id>12)&&abc<=13||xyz>=42"
        @?= Success "" (Write longExpr)

unit_inlineIf :: Assertion
unit_inlineIf = do
    runParser parseL "if cond then x = x + 1" @?= Success ""
        (If (Ident "cond") (Assign "x" $ BinOp Plus (Ident "x") (Num 1)) (Seq []))
    runParser parseL "if cond then x = x + 1 else read y, write z" @?= Success ""
        (If (Ident "cond") (Assign "x" $ BinOp Plus (Ident "x") (Num 1)) (Seq [
            Read "y", Write (Ident "z")
        ]))

unit_inlineWhile :: Assertion
unit_inlineWhile = do
    runParser parseL "while cond do write 1" @?= Success ""
        (While (Ident "cond") (Write (Num 1)))


unit_fullIf :: Assertion
unit_fullIf = do
    let program1 = unlines $
            [ "if cond then"
            , "  read x"
            , "else"
            , "  read y"
            ]
    runParser parseL program1 @?= Success ""
        (If (Ident "cond") (Read "x") (Read "y"))
    let program2 = unlines $
            [ "if cond then"
            , "  read x"
            ]
    runParser parseL program2 @?= Success ""
        (If (Ident "cond") (Read "x") (Seq []))
    
unit_fullWhile :: Assertion
unit_fullWhile = do
    let program = unlines $
            [ "while cond do"
            , " read x"
            ]
    runParser parseL program @?= Success ""
        (While (Ident "cond") (Read "x"))

unit_sampleProgram :: Assertion
unit_sampleProgram = do
    let program = unlines $
            [ "read n"
            , "i = 2, ans = 0"
            , "while i*i <= n do"
            , "  if n - n / i * i == 0 then"
            , "    ans = 1, i = n"
            , "  i = i + 1"
            , "write ans"
            ]
    longE <- parse E.parseExpr "n-n/i*i==0" 
    return ()
    runParser parseL program @?= Success ""
        (Seq 
        [ Read "n"
        , Seq [Assign "i" (Num 2), Assign "ans" (Num 0)]
        , While (BinOp Le (BinOp Mult (Ident "i") (Ident "i")) (Ident "n")) (Seq
            [ If longE (Seq [Assign "ans" (Num 1), Assign "i" (Ident "n")]) (Seq [])
            , Assign "i" (BinOp Plus (Ident "i") (Num 1))
            ])
        , Write (Ident "ans")
        ])