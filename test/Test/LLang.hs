module Test.LLang where

import           AST
import           Combinators      (Parser, Result (..), elem', parse, runParser,
                                   satisfy, sepBy1, symbol)
import qualified Expr             as E
import           LLang

import qualified Data.Map         as Map
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))

isFailure (Failure _) = True
isFailure _           = False

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

-- read x;
-- if (x > 13)
-- then { write x }
-- else {
--     while (x < 42) {
--       x := x * 7;
--       write (x);
--     }
-- }
stmt1 :: LAst
stmt1 =
  Seq
    [ Read "X"
    , If (BinOp Gt (Ident "X") (Num 13))
         (Write (Ident "X"))
         (While (BinOp Lt (Ident "X") (Num 42))
                (Seq [ Assign "X"
                        (BinOp Mult (Ident "X") (Num 7))
                     , Write (Ident "X")
                     ]
                )
         )
    ]

unit_stmt1 :: Assertion
unit_stmt1 = do
  let xIs n = Map.fromList [("X", n)]
  eval stmt1 (initialConf [1]) @?= Just (Conf (xIs 49) [] [49, 7])
  eval stmt1 (initialConf [10]) @?= Just (Conf (xIs 70) [] [70])
  eval stmt1 (initialConf [42]) @?= Just (Conf (xIs 42) [] [42])


-- read x;
-- if (x)
-- then {
--   while (x) {
--     x := x - 2;
--     write (x);
--   }
-- else {}
stmt2 :: LAst
stmt2 =
  Seq
    [ Read "x"
    , If (Ident "x")
         (While (Ident "x")
                (Seq
                   [ (Assign "x" (BinOp Minus (Ident "x") (Num 2)))
                   , (Write (Ident "x"))
                   ]
                )
         )
         (Seq [])
    ]

unit_stmt2 :: Assertion
unit_stmt2 = do
  let xIs n = Map.fromList [("x", n)]
  eval stmt2 (initialConf [0]) @?= Just (Conf (xIs 0) [] [])
  eval stmt2 (initialConf [2]) @?= Just (Conf (xIs 0) [] [0])
  eval stmt2 (initialConf [42]) @?= Just (Conf (xIs 0) [] (filter even [0 .. 40]))

-- read x;
-- read y;
-- write (x == y);
stmt3 :: LAst
stmt3 =
  Seq
    [ Read "x"
    , Read "y"
    , Write (BinOp Equal (Ident "x") ((Ident "y")))
    ]

unit_stmt3 :: Assertion
unit_stmt3 = do
  let subst x y = Map.fromList [("x", x), ("y", y) ]
  eval stmt3 (initialConf [0, 2]) @?= Just (Conf (subst 0 2) [] [0])
  eval stmt3 (initialConf [2, 2]) @?= Just (Conf (subst 2 2) [] [1])
  eval stmt3 (initialConf [42]) @?= Nothing

-- read n;
-- if (n == 1 || n == 2)
-- then {
--   write 1;
-- }
-- else {
--   i := 2;
--   cur := 1
--   prev := 1
--   while (i < n) {
--     temp := cur + prev;
--     prev := cur;
--     cur := temp;
--     i := i + 1;
--   }
--   write (cur);
-- }
stmt4 :: LAst
stmt4 =
  Seq
    [ Read "n"
    , If (BinOp Or (BinOp Equal (Ident "n") (Num 1)) (BinOp Equal (Ident "n") (Num 2)))
         (Write (Num 1))
         (Seq
            [ Assign "i" (Num 2)
            , Assign "cur" (Num 1)
            , Assign "prev" (Num 1)
            , While (BinOp Lt (Ident "i") (Ident "n"))
                     (Seq
                        [ Assign "temp" (BinOp Plus (Ident "cur") (Ident "prev"))
                        , Assign "prev" (Ident "cur")
                        , Assign "cur" (Ident "temp")
                        , Assign "i" (BinOp Plus (Ident "i") (Num 1))
                        ]
                     )
            , Write (Ident "cur")
            ]
         )
    ]

unit_stmt4 :: Assertion
unit_stmt4 = do
  let subst n i cur prev temp = Map.fromList [("n", n), ("i", i), ("cur", cur), ("prev", prev), ("temp", temp)]
  let subst' n = Map.fromList [("n", n)]
  eval stmt4 (initialConf [1]) @?= Just (Conf (subst' 1) [] [1])
  eval stmt4 (initialConf [2]) @?= Just (Conf (subst' 2) [] [1])
  eval stmt4 (initialConf [10]) @?= Just (Conf (subst 10 10 55 34 55) [] [55] )
  eval stmt4 (initialConf []) @?= Nothing
