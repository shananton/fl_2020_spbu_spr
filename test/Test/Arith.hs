module Test.Arith where 

import Test.HUnit (Assertion, (@?=))
import Arith (evaluate, parseNum, parseOp, parseSum, parseMult, Operator (..), AST (..), toPostfix, fromPostfix)

unit_evaluate :: Assertion 
unit_evaluate = do 
    evaluate "1" @?= Just 1 
    evaluate "1+2" @?= Just (1+2)
    evaluate "2+4+8" @?= Just (2+4+8)
    evaluate "11+22" @?= Just (11+22)
    evaluate "13+42+777" @?= Just (13+42+777)
    evaluate "31+24+777" @?= Just (31+24+777)
    evaluate "1+2*3+4" @?= Just (1+2*3+4)
    evaluate "12+23*34+456" @?= Just (12+23*34+456)
    evaluate "1-2*3+4" @?= Just (1-(2*3+4))
    evaluate "1-2-3" @?= Just (1-(2-3))
    evaluate "4/2-2" @?= Just ((4 `div` 2) - 2)
    evaluate "(1+2)*(3+4)" @?= Just ((1+2)*(3+4))
    evaluate "12+(23*(34)+456)" @?= Just (12+(23*(34)+456))
    evaluate "((1-(2*3))+4)" @?= Just ((1-(2*3))+4)

unit_parseNum :: Assertion 
unit_parseNum = do 
    parseNum "7" @?= Just (Num 7, "")
    parseNum "12+3" @?= Just (Num 12, "+3")
    parseNum "007" @?= Just (Num 7, "")
    parseNum "+3" @?= Nothing 
    parseNum "a" @?= Nothing 

unit_parseOp :: Assertion 
unit_parseOp = do 
    parseOp "+1" @?= Just (Plus, "1")
    parseOp "**" @?= Just (Mult, "*")
    parseOp "-2" @?= Just (Minus, "2")
    parseOp "/1" @?= Just (Div, "1")
    parseOp "12" @?= Nothing 

unit_parseMult :: Assertion 
unit_parseMult = do 
    parseMult "1*2*3" @?= Just (BinOp Mult (Num 1) (BinOp Mult (Num 2) (Num 3)), "")
    parseMult "123" @?= Just (Num 123, "")
    parseMult "1*2+3*4" @?= Just (BinOp Mult (Num 1) (Num 2), "+3*4")

unit_parseSum :: Assertion 
unit_parseSum = do 
    parseSum "1*2*3"   @?= Just (BinOp Mult (Num 1) (BinOp Mult (Num 2) (Num 3)), "")
    parseSum "123"     @?= Just (Num 123, "")
    parseSum "1*2+3*4" @?= Just (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (BinOp Mult (Num 3) (Num 4)), "")
    parseSum "1+2*3+4" @?= Just (BinOp Plus (Num 1) (BinOp Plus (BinOp Mult (Num 2) (Num 3)) (Num 4)), "")

unit_toPostfix :: Assertion
unit_toPostfix = do
    toPostfix (Num 123) @?= "123"
    toPostfix (BinOp Plus (Num 13) (Num 42)) @?= "13 42 +"
    toPostfix (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (Num 3)) @?= "1 2 * 3 +"
    toPostfix (BinOp Plus (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (BinOp Div (Num 3) (Num 4))) (BinOp Plus (Num 5) (BinOp Minus (Num 6) (Num 7)))) @?= "1 2 - 3 4 / * 5 6 7 - + +"

unit_fromPostfix :: Assertion 
unit_fromPostfix = do 
    fromPostfix "123" @?= Just (Num 123) 
    fromPostfix "1 2 * 3 +" @?= Just (BinOp Plus (BinOp Mult (Num 1) (Num 2)) (Num 3)) 
    fromPostfix "1 2 - 3 4 / * 5 6 7 - + +" @?= Just (BinOp Plus (BinOp Mult (BinOp Minus (Num 1) (Num 2)) (BinOp Div (Num 3) (Num 4))) (BinOp Plus (Num 5) (BinOp Minus (Num 6) (Num 7)))) 
    fromPostfix "13 42 +" @?= Just (BinOp Plus (Num 13) (Num 42))
    fromPostfix "1 2 3 +" @?= Nothing
    fromPostfix "1 2 + *" @?= Nothing 