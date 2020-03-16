module Combinators where

import Control.Applicative
import Control.Monad
import Control.Monad.Fail

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

instance Functor (Result error input) where
  fmap f (Success i r) = Success i (f r)
  fmap f (Failure e)   = Failure e

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap f = Parser . fmap (fmap f) . runParser

instance Applicative (Parser error input) where
  pure x = Parser $ \s -> Success s x
  Parser p1 <*> Parser p2 = Parser $ \s -> case p1 s of
    Failure e   -> Failure e
    Success i r -> r <$> p2 i

instance Monad (Parser error input) where
  Parser x >>= k = Parser $ \s -> case x s of
    Failure e   -> Failure e
    Success i r -> runParser (k r) i

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ const $ Failure mempty
  Parser p1 <|> Parser p2 = Parser $ \s -> case p1 s of
    Failure e -> case p2 s of
      Failure e' -> Failure $ e <> e'
      x          -> x
    x         -> x

instance Monoid error => MonadPlus (Parser error input)

instance Monoid error => MonadFail (Parser error input) where
  fail _ = empty

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem <*> many (sep *> elem)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = satisfy (== c)

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: (Show a) => Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Show a => (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \input ->
  case input of
    (x:xs) | p x -> Success xs x
    []           -> Failure "Empty string"
    (x:xs)       -> Failure "Predicate failed"

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure
