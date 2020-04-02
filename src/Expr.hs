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

data OpType = Binary Associativity
            | Unary

-- Универсальный парсер выражений
uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr allOps atom astBin astUn = uber allOps
  where
    astr l o r = astBin o l r
    astl o r l = astBin o l r
    uber ((op, opType):ops) = let term = uber ops in
      case opType of
        Binary LeftAssoc  -> foldl (&) <$> term <*> many (astl <$> op <*> term)
        Binary RightAssoc -> flip (foldr ($)) <$> many (astr <$> term <*> op) <*> term
        Binary NoAssoc    -> astr <$> term <*> op <*> term <|> term
        Unary             -> flip (foldr ($)) <$> many (astUn <$> op) <*> term
    uber [] = atom

expr :: Parser String [Token] AST
expr = uberExpr ops atom BinOp UnaryOp
  where
    ops = map (first listToParser)
              [ ([Or], Binary RightAssoc)
              , ([And], Binary RightAssoc)
              , ([Not], Unary)
              , ([Equal, Nequal, Lt, Le, Gt, Ge], Binary NoAssoc)
              , ([Plus, Minus], Binary LeftAssoc)
              , ([Mult, Div], Binary LeftAssoc)
              , ([Minus], Unary)
              , ([Pow], Binary RightAssoc)
              ]
      where
        listToParser = fmap (getArith . getOperator) . getAlt
          . foldMap (Alt . symbol . TOperator . Arith)
    atom = Num . getInt <$> satisfy isInt
        <|> Ident . getId <$> satisfy isId
        <|> symbol (TOperator LPar) *> expr <* symbol (TOperator RPar)

-- Парсеры, чтобы пройти тесты -_-


arithRepr :: EnumStringRepr Operator
arithRepr = 
  [ (Plus, "+")
  , (Mult, "*")
  , (Minus, "-")
  , (Div, "/")
  , (Pow, "^")
  , (Equal, "==")
  , (Nequal, "/=")
  , (Gt, ">")
  , (Ge, ">=")
  , (Lt, "<")
  , (Le, "<=")
  , (And, "&&")
  , (Or, "||")
  , (Not, "!")
  ]

parseExpr :: Parser String String AST
parseExpr = uberExpr ops atom BinOp UnaryOp
  where
    ops = map (first listToParser)
              [ ([Or], Binary RightAssoc)
              , ([And], Binary RightAssoc)
              , ([Not], Unary)
              , ([Equal, Nequal, Le, Lt, Ge, Gt], Binary NoAssoc)
              , ([Plus, Minus], Binary LeftAssoc)
              , ([Mult, Div], Binary LeftAssoc)
              , ([Minus], Unary)
              , ([Pow], Binary RightAssoc)
              ]
      where
        listToParser = getAlt . foldMap (Alt . elemP)
          where
            elemP x = x <$ string (fromJust $ lookup x arithRepr)
    atom = Num <$> nat 
      <|> Ident <$> ident 
      <|> symbol '(' *> parseExpr <* symbol ')'

-- parseExpr :: Parser String String AST
-- parseExpr = Parser
--   $ maybe (Failure "parseExpr failed") (Success "")
--   . (parseMaybe lexAll >=> parseMaybe (expr <* symbol (TSep Newline)))

-- Парсер для целых чисел
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
