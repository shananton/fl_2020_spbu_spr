module Test.LLang where

import           AST
import           Combinators      (Parser, Result (..), parse, runParser,
                                   satisfy, sepBy1, symbol, parse_)
import qualified Expr             as E
import           LLang
import           Lexer (parseRawEither)

import           Control.Applicative (some)
import qualified Data.Map         as Map
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))

testFailure = assertBool "" . isFailure

isFailure (Left _) = True
isFailure _        = False

code = toSeq <$> some stmt

unit_basic :: Assertion
unit_basic = do
    parseRawEither code "x=1" @?= Right (Assign "x" (Num 1))
    parseRawEither code "x  =   2  " @?= Right (Assign "x" (Num 2))
    assertBool "" $ isFailure $ parseRawEither code " x=2"

    parseRawEither code "read var_name" @?= Right (Read "var_name")
    assertBool "" $ isFailure $ parseRawEither code "read read"
    parseRawEither code "read read_val" @?= Right (Read "read_val")
    parseRawEither code "write 1" @?= Right (Write (Num 1))


unit_lexpr :: Assertion
unit_lexpr = do
    parseRawEither code "write 1+2" @?= Right (Write (BinOp Plus (Num 1) (Num 2)))
    parseRawEither code "write 1+-2" @?= Right (Write (BinOp Plus (Num 1) (UnaryOp Minus (Num 2))))
    parseRawEither code "write --1" @?= Right (Write (UnaryOp Minus $ UnaryOp Minus $ Num 1))
    parseRawEither code "write !flag" @?= Right (Write $ UnaryOp Not $ Ident "flag")

    longExpr <- parse_ E.parseExpr "(1==x+2)||3*4<y-5/6&&(7/=z^8)||(id>12)&&abc<=13||xyz>=42"
    parseRawEither code "write (1==x+2)||3*4<y-5/6&&(7/=z^8)||(id>12)&&abc<=13||xyz>=42"
        @?= Right (Write longExpr)

unit_inlineIf :: Assertion
unit_inlineIf = do
    parseRawEither code "if cond then x = x + 1" @?= Right
        (If (Ident "cond") (Assign "x" $ BinOp Plus (Ident "x") (Num 1)) (Seq []))
    parseRawEither code "if cond then x = x + 1 else read y, write z" @?= Right
        (If (Ident "cond") (Assign "x" $ BinOp Plus (Ident "x") (Num 1)) (Seq [
            Read "y", Write (Ident "z")
        ]))

unit_inlineWhile :: Assertion
unit_inlineWhile =
    parseRawEither code "while cond do write 1" @?= Right
    (While (Ident "cond") (Write (Num 1)))


unit_fullIf :: Assertion
unit_fullIf = do
    let program1 = unlines
            [ "if cond then"
            , "  read x"
            , "else"
            , "  read y"
            ]
    parseRawEither code program1 @?= Right
        (If (Ident "cond") (Read "x") (Read "y"))
    let program2 = unlines
            [ "if cond then"
            , "  read x"
            ]
    parseRawEither code program2 @?= Right
        (If (Ident "cond") (Read "x") (Seq []))

unit_fullWhile :: Assertion
unit_fullWhile = do
    let program = unlines
            [ "while cond do"
            , " read x"
            ]
    parseRawEither code program @?= Right
        (While (Ident "cond") (Read "x"))

unit_sampleProgram :: Assertion
unit_sampleProgram = do
    let program = unlines
            [ "read n"
            , "i = 2, ans = 0"
            , "while i*i <= n do"
            , "  if n - n / i * i == 0 then"
            , "    ans = 1, i = n"
            , "  i = i + 1"
            , "write ans"
            ]
    longE <- parse_ E.parseExpr "n-n/i*i==0"
    parseRawEither code program @?= Right
        (Seq
        [ Read "n"
        , Assign "i" (Num 2)
        , Assign "ans" (Num 0)
        , While (BinOp Le (BinOp Mult (Ident "i") (Ident "i")) (Ident "n")) (Seq
            [ If longE (Seq [Assign "ans" (Num 1), Assign "i" (Ident "n")]) (Seq [])
            , Assign "i" (BinOp Plus (Ident "i") (Num 1))
            ])
        , Write (Ident "ans")
        ])

unit_functionDef = do
  let defFullNoArgs = unlines
          [ "foo() is"
          , "  read x"
          , "  write x"
          ]
  parseRawEither function defFullNoArgs @?= Right (Function "foo" []
      (Seq
      [ Read "x"
      , Write (Ident "x")
      ]
      )
    )
  let defFullArgs = unlines
          [ "bar(x, y, z) is"
          , "  x + y"
          , "  return foo(z + x)"
          ]
  parseRawEither function defFullArgs @?= Right (Function "bar" ["x", "y", "z"]
      (Seq
      [ Assign "_" $ BinOp Plus (Ident "x") (Ident "y")
      , Return $ FunctionCall "foo" [BinOp Plus (Ident "z") (Ident "x")]
      ]
      )
    )

  let defShort = "log(b, a) = ln(a) / ln(b)"
  parseRawEither function defShort @?= Right (Function "log" ["b", "a"] $
      Return $ BinOp Div (FunctionCall "ln" [Ident "a"]) (FunctionCall "ln" [Ident "b"])
    )

  let defEmpty = unlines
        [ "me() is"
        , "  pass"
        ]
  parseRawEither function defEmpty @?= Right (Function "me" [] (Seq []))

unit_program = do
  let programCorrect = unlines
        [ "foo(x) = x^10 + ln(x)"
        , "main(bosow, notBosow) = bosow"
        , "hello() is"
        , "  write 1337"
        , "main() is"
        , "  read n"
        , "  foo(n)"
        ]
  parseRawEither program programCorrect @?= Right (Program
      [ Function "foo" ["x"] $
          Return $ BinOp Plus (BinOp Pow (Ident "x") (Num 10))
            (FunctionCall "ln" [Ident "x"])
      , Function "main" ["bosow", "notBosow"] $
          Return $ Ident "bosow"
      , Function "hello" [] $
          Write $ Num 1337
      ] $ Seq
      [ Read "n"
      , Assign "_" $ FunctionCall "foo" [Ident "n"]
      ]
    )
  let programMultipleDefs = unlines
        [ "foo() = 1"
        , "foo() = 2"
        , "main() = 0"
        ]
  testFailure $ parseRawEither program programMultipleDefs
  let programNoMain = "foo() = 1"
  testFailure $ parseRawEither program programNoMain

