{-# LANGUAGE LambdaCase #-}

module Combinators where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail

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
    Failure e -> Failure e
    Success i r -> r <$> p2 i

instance Monad (Parser error input) where
  Parser x >>= k = Parser $ \s -> case x s of
    Failure e -> Failure e
    Success i r -> runParser (k r) i

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ const $ Failure mempty

  Parser p1 <|> Parser p2 = Parser $ \s -> case p1 s of
    Failure e -> case p2 s of
      Failure e' -> Failure $ e <> e'
      x -> x
    x -> x

instance Monoid error => MonadPlus (Parser error input)

instance Monoid error => MonadFail (Parser error input) where
  fail _ = empty

-- Применяет парсер ко всей последовательности
parseMaybe :: Parser e [t] a -> [t] -> Maybe a
parseMaybe p i = case runParser p i of
  Success [] x -> Just x
  _            -> Nothing

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem
  <*> many (sep *> elem)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Eq a => a -> Parser String [a] a
symbol c = satisfy (== c)

-- Принимает заданную строку
string :: String -> Parser String String String
string = foldr ((<*>) . fmap (:) . symbol) (pure "")

-- Успешно завершается, если последовательность содержит как минимум один элемент
elem' :: Parser String [a] a
elem' = satisfy (const True)

-- Проверяет, что первый элемент принадлежит списку
oneOf :: Eq a => [a] -> Parser String [a] a
oneOf = satisfy . flip elem

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \case
  (x:xs)
    | p x -> Success xs x
  [] -> Failure "Empty string"
  _ -> Failure "Predicate failed"

-- Возвращает первый символ, оставляя его во входной последовательности.
peek :: Parser String String Char
peek = scout (const True)

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату,
-- оставляя его во входной последовательности.
scout :: (a -> Bool) -> Parser String [a] a
scout p = Parser $ \case
  xs@(x:_)
    | p x -> Success xs x
  [] -> Failure "Empty string"
  _ -> Failure "Predicate failed"

-- Успешен тогда и только тогда, когда переданный парсер завершается неудачей.
-- Не изменяет входную последовательность.
shouldFail :: Parser e i r -> Parser String i ()
shouldFail p = Parser $ \input -> case runParser p input of
  Success _ _ -> Failure "Avoided parser succeeded"
  _ -> Success input ()

-- Проверяет, что в начале входного потока нет символа, удовлетворяющего предикату
avoid :: (a -> Bool) -> Parser String [a] ()
avoid = shouldFail . scout

-- Игнорирует результат парсера
ignore :: Parser e i a -> Parser e i ()
ignore = (() <$)

-- Применяет парсер, при неудаче возвращая значение по умолчанию
option :: Monoid e => a -> Parser e i a -> Parser e i a
option x = (<|> pure x)

-- Версия option_, игнорирующая результат
option_ :: Monoid e => Parser e i a -> Parser e i ()
option_ = option () . ignore

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure
