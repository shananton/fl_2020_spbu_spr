module Expr where

import           AST                 (AST (..), Operator (..))
import           Combinators
import           Lexer

import           Control.Applicative (many, some, (<|>))
import           Control.Monad       ((>=>))
import           Control.Monad.State (StateT (..))
import           Data.Bifunctor      (first)
import           Data.Char           (digitToInt, isAlpha, isAlphaNum, isDigit,
                                      isHexDigit)
import           Data.Function       ((&))
import           Data.Functor        (($>))
import           Data.Maybe          (fromJust, maybe)
import           Data.Monoid         (Alt (..))
import           Data.Tuple          (swap)

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
    uber ((op, assoc):ops) = let term = uber ops in
      case assoc of
        LeftAssoc  -> foldl (&) <$> term <*> many (astl <$> op <*> term)
        RightAssoc -> flip (foldr ($)) <$> many (astr <$> term <*> op) <*> term
        NoAssoc    -> astr <$> term <*> op <*> term <|> term
    uber [] = atom

expr :: Parser String [Token] AST
expr = uberExpr ops atom ast
  where
    ops = map (first listToParser)
              [ ([Or], RightAssoc)
              , ([And], RightAssoc)
              , ([Equal, Nequal, Lt, Le, Gt, Ge], NoAssoc)
              , ([Plus, Minus], LeftAssoc)
              , ([Mult, Div], LeftAssoc)
              , ([Pow], RightAssoc)
              ]
      where
        listToParser = fmap (getArith . getOp) . getAlt
          . foldMap (Alt . symbol . TOperator . Arith)
    atom = signedNum
        <|> Ident . getId <$> satisfy isId
        <|> symbol (TSep LPar) *> expr <* symbol (TSep RPar)
      where
        signedNum = Num <$> (flip (foldr ($))
            <$> many (negate <$ symbol (TOperator (Arith Minus)))
            <*> (getInt <$> satisfy isInt))
    ast = BinOp

-- Парсеры, чтобы пройти тесты -_-

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = Parser
  $ maybe (Failure "parseExpr failed") (Success "")
  . (parseMaybe lexAll >=> parseMaybe (expr <* symbol (TSep Newline)))

parseNum :: Parser String String Int
parseNum = flip (foldr ($)) <$> many (negate <$ symbol '-') <*> nat

parseIdent :: Parser String String String
parseIdent = ident

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
evaluate input = case runParser parseExpr input of
  Success rest ast
    | null rest -> return $ compute ast
  _ -> Nothing

compute :: AST -> Int
compute (Num x)           = x
compute (BinOp Plus x y)  = compute x + compute y
compute (BinOp Mult x y)  = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y)   = compute x `div` compute y
compute (BinOp Pow x y)   = compute x ^ compute y

