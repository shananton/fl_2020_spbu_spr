module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
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
import qualified Data.Map as Map

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


-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = error "parseExpr undefined"
