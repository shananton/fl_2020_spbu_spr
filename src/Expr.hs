module Expr where

import AST                 (AST (..), Operator (..))
import Combinators         (Parser (..), Result (..), symbol, satisfy, elem', fail')
import Data.Char           (digitToInt, isDigit)
import Control.Applicative (some, many, (<|>))
import Data.Function       ((&))

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

-- Универсальный парсер выражений
uberExpr :: Monoid e
         => [(Parser e i op, Associativity)] -- список парсеров бинарных операторов с ассоциативностями в порядке повышения приоритета
         -> Parser e i ast -- парсер для элементарного выражения
         -> (op -> ast -> ast -> ast) -- функция для создания абстрактного синтаксического дерева для бинарного оператора
         -> Parser e i ast
uberExpr allOps atom ast = uber allOps
  where
  astr l o r = ast o l r
  astl o r l = ast o l r
  uber ((op, assoc):ops) = let term = uber ops in case assoc of
    LeftAssoc  -> foldl (&) <$> term <*> many (astl <$> op <*> term)
    RightAssoc -> flip (foldr ($)) <$> many (astr <$> term <*> op) <*> term
    NoAssoc    -> astr <$> term <*> op <*> term <|> term
  uber []                = atom

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = uberExpr ops term BinOp
  where
  opMult   = symbol '*' >>= toOperator
  opDiv    = symbol '/' >>= toOperator
  opPlus   = symbol '+' >>= toOperator
  opMinus  = symbol '-' >>= toOperator
  opPow    = symbol '^' >>= toOperator
  ops = [ (opPlus <|> opMinus, LeftAssoc)
        , (opMult <|> opDiv, LeftAssoc)
        , (opPow, RightAssoc)
        ]
  term = Num <$> parseNum <|> symbol '(' *> parseExpr <* symbol ')'

-- Парсер для целых чисел
parseNum :: Parser String String Int
parseNum = foldl (\acc d -> 10 * acc + digitToInt d) 0 <$> some (satisfy isDigit)

-- Парсер для идентификаторов
parseIdent :: Parser String String String
parseIdent = error "parseIdent undefined"

-- Парсер для операторов
parseOp :: Parser String String Operator
parseOp = elem' >>= toOperator

-- Преобразование символов операторов в операторы
toOperator :: Char -> Parser String String Operator
toOperator '+' = pure Plus
toOperator '*' = pure Mult
toOperator '-' = pure Minus
toOperator '/' = pure Div
toOperator '^' = pure Pow
toOperator _   = fail' "Failed toOperator"

evaluate :: String -> Maybe Int
evaluate input = do
  case runParser parseExpr input of
    Success rest ast | null rest -> return $ compute ast
    _                            -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute (BinOp Pow x y)   = compute x ^ compute y

