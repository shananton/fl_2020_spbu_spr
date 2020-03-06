module Combinators where

import           Control.Applicative

data Result error input result
  = Success input result
  | Failure error
  deriving (Show, Eq)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

instance Functor (Parser error input) where
  fmap = error "fmap not implemented"

instance Applicative (Parser error input) where
  pure = error "pure not implemented"
  (<*>) = error "<*> not implemented"

instance Monad (Parser error input) where
  return = error "return not implemented"

  (>>=) = error ">>= not implemented"

instance Monoid error => Alternative (Parser error input) where
  empty = error "empty not implemented"

  (<|>) = error "<|> not implemented"

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = error "sepBy1 not implemented"

-- Альтернатива: в случае неудачи разбора первым парсером, парсит вторым
alt' :: Parser e i a -> Parser e i a -> Parser e i a
alt' p q = Parser $ \input ->
  case runParser p input of
    Failure _ -> runParser q input
    x         -> x

-- Последовательное применение парсеров:
-- если первый парсер успешно принимает префикс строки, второй запускается на суффиксе.
-- Второй парсер использует результат первого.
bind' :: Parser e i a
      -> (a -> Parser e i b)
      -> Parser e i b
bind' p f = Parser $ \input ->
  case runParser p input of
    Success i r -> runParser (f r) i
    Failure e   -> Failure e

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
    []           -> Failure $ "Empty string"
    (x:xs)       -> Failure $ "Predicate failed"

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success a = Parser $ \input -> Success input a

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' = Parser . const . Failure

-- Проверяет, что первый элемент входной последовательности -- данный символ
fmap' :: (a -> b) -> Parser e i a -> Parser e i b
fmap' f p = Parser $ \input ->
  case runParser p input of
    Success i a -> Success i (f a)
    Failure e   -> Failure e

-- Последовательное применения одного и того же парсера 1 или более раз
some' :: Parser e i a -> Parser e i [a]
some' p =
  p `bind'` \a ->
  many' p `bind'` \as ->
  success (a : as)

-- Последовательное применение одного и того же парсера 0 или более раз
many' :: Parser e i a -> Parser e i [a]
many' p =
  some' p `alt'` success []
