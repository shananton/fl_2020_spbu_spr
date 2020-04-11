module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators         (Parser (..), Result (..), fail',
                                      runParser, satisfy, stream, success)
import           Control.Applicative
import           Data.Char           (digitToInt, isDigit)
import qualified Data.Map            as Map

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr = error "evalExpr undefined"

uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr = error "uberExpr undefined"


-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = error "parseExpr undefined"

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> go
  where
    go :: Parser String String String
    go = some (satisfy isDigit)

parseNegNum :: Parser String String Int
parseNegNum = error "parseNegNum undefined"

parseIdent :: Parser String String String
parseIdent = error "parseIdent undefined"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null (stream rest) -> return $ compute ast
    _                                     -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y

