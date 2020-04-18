module Test.LEval where

import           AST
import qualified Data.Map         as Map
import           Data.Maybe       (isNothing)
import           LEval            (evalProg)
import           LLang            (Configuration (..), Function (..), LAst (..),
                                   Program (..))
import           Test.Tasty.HUnit (Assertion, assertBool, (@?=))
import           Text.Printf      (printf)

ignoreDefs :: Maybe Configuration -> Maybe Configuration -> Assertion
ignoreDefs (Just conf) (Just conf') = do
    subst conf @?= subst conf'
    input conf @?= input conf'
    output conf @?= output conf'
ignoreDefs Nothing conf = assertBool "" (isNothing conf)
ignoreDefs conf Nothing = assertBool "" (isNothing conf)



-- f x y = read z ; return (x + z * y)
-- g x = if (x) then res = y else res = x*13; return res
-- {read x; read y; write (f x y); write (g x)}"
prog =
  Program
    [ Function "f" ["x", "y"] (Read "z") (BinOp Plus (Ident "x") (BinOp Mult (Ident "z") (Ident "y")))
    , Function "g" ["x"] (If (Ident "x")
                             (Seq [Write (Num 42), Assign "res" (Ident "x")])
                             (Assign "res" (BinOp Mult (Ident "x") (Num 13))))
                         (Ident "res")
    ]
    (
      Seq
        [ Read "x"
        , Read "y"
        , Write (FunctionCall "f" [Ident "x", Ident "y"])
        , Write (FunctionCall "g" [Ident "x"])
        ]
    )

unit_evalProg = do
    let vars x y = Map.fromList [("x", x), ("y", y)]
    ignoreDefs (evalProg prog [1..10])    (Just $ Conf (vars 1 2) [4..10] [1, 42, 7] Map.empty)
    ignoreDefs (evalProg prog [13, 1, 2]) (Just $ Conf (vars 13 1) [] [13, 42, 15] Map.empty)
    ignoreDefs (evalProg prog [0, 1, 2])  (Just $ Conf (vars 0 1) [] [0, 2] Map.empty)

-- f x = if (x < 10) then {x := x + 1; write x; r := f x} else {}; return 0
-- {read x; _ := f x}
prog1 :: Program
prog1 =
  Program
    [ Function "f" ["x"]
        (If (BinOp Lt (Ident "x") (Num 10))
            (Seq [ Assign "x" (BinOp Plus (Ident "x") (Num 1))
                 , Write (Ident "x")
                 , Assign "r" (FunctionCall "f" [Ident "x"])
                 ]
            )
            (Seq []))
        (Num 0)
    ]
    (
      Seq
        [ Read "x"
        , Assign "_" (FunctionCall "f" [Ident "x"])
        ]
    )

unit_evalProg1 = do
    let vars x y = Map.fromList [("x", x), ("_", y)]
    ignoreDefs (evalProg prog1 [1..10]) (Just $ Conf (vars 1 0) [2..10] (reverse [2..10]) Map.empty)
    ignoreDefs (evalProg prog1 [13]) (Just $ Conf (vars 13 0) [] [] Map.empty)

-- f x = if (x < 10) then {x := x + 1; r := f x; write x} else {}; return 0
-- {read x; _ := f x}
prog2 :: Program
prog2 =
  Program
    [ Function "f" ["x"]
        (If (BinOp Lt (Ident "x") (Num 10))
            (Seq [ Assign "x" (BinOp Plus (Ident "x") (Num 1))
                 , Assign "r" (FunctionCall "f" [Ident "x"])
                 , Write (Ident "x")
                 ]
            )
            (Seq []))
        (Num 0)
    ]
    (
      Seq
        [ Read "x"
        , Assign "_" (FunctionCall "f" [Ident "x"])
        ]
    )

unit_evalProg2 = do
    let vars x y = Map.fromList [("x", x), ("_", y)]
    ignoreDefs (evalProg prog2 [1..10]) (Just $ Conf (vars 1 0) [2..10] [2..10] Map.empty)
    ignoreDefs (evalProg prog2 [13]) (Just $ Conf (vars 13 0) [] [] Map.empty)
    ignoreDefs (evalProg prog2 []) Nothing

-- even n = if n == 0 then {res = 0} else {res = odd (n - 1)}; return res
-- odd n = if n == 0 then {res = 1} else {if n == 1 then {res = 0} else {res = even (n - 1)}}; return res
-- {read n; write (even n); write (odd n)}
prog3 =
  Program
    [ Function "even" ["n"]
        (If (BinOp Equal (Ident "n") (Num 0))
            (Assign "res" (Num 0))
            (Assign "res" (FunctionCall "odd" [BinOp Minus (Ident "n") (Num 1)]))
        )
        (Ident "res")
    , Function "odd" ["n"]
        (If (BinOp Equal (Ident "n") (Num 0))
            (Assign "res" (Num 1))
            (If (BinOp Equal (Ident "n") (Num 1))
                (Assign "res" (Num 0))
                (Assign "res" (FunctionCall "even" [BinOp Minus (Ident "n") (Num 1)]))
            )
        )
        (Ident "res")
    ]
    (Seq
      [ Read "n"
      , Write (FunctionCall "even" [Ident "n"])
      , Write (FunctionCall "odd" [Ident "n"])
      ]
    )

unit_evalProg3 = do
    let vars n = Map.fromList [("n", n)]
    ignoreDefs (evalProg prog3 [1..10]) (Just $ Conf (vars 1) [2..10] [0, 1] Map.empty)
    ignoreDefs (evalProg prog3 [13]) (Just $ Conf (vars 13) [] [0, 1] Map.empty)
    ignoreDefs (evalProg prog3 [0]) (Just $ Conf (vars 0) [] [1, 0] Map.empty)


-- even n = if n == 0 then {res = 0} else {res = odd (n - 1)}; return res
-- odd n = if n == 0 then {res = 1} else {if n == 1 then {res = 0} else {res = even (n - 1)}}; return res
-- {write(odd 0)}
prog4 =
  Program
    [ Function "even" ["n"]
        (If (BinOp Equal (Ident "n") (Num 0))
            (Assign "res" (Num 0))
            (Assign "res" (FunctionCall "odd" [BinOp Minus (Ident "n") (Num 1)]))
        )
        (Ident "res")
    , Function "odd" ["n"]
        (If (BinOp Equal (Ident "n") (Num 0))
            (Assign "res" (Num 1))
            (If (BinOp Equal (Ident "n") (Num 1))
                (Assign "res" (Num 0))
                (Assign "res" (FunctionCall "even" [BinOp Minus (Ident "n") (Num 1)]))
            )
        )
        (Ident "res")
    ]
    (Seq [ Write (FunctionCall "odd" [Num 0])
         , Write (FunctionCall "odd" [Num 1])
         , Write (FunctionCall "odd" [Num 2])
         , Write (FunctionCall "odd" [Num 13])
         , Write (FunctionCall "even" [Num 0])
         , Write (FunctionCall "even" [Num 1])
         , Write (FunctionCall "even" [Num 2])
         , Write (FunctionCall "even" [Num 13])
         ]
    )

unit_evalProg4 = do
    ignoreDefs (evalProg prog4 []) (Just $ Conf Map.empty [] [1,0,1,0,0,1,0,1] Map.empty)
