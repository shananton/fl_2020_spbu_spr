{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Combinators where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail
import           Data.List           (nub, sortOn)
import           Control.Monad.Except

data Result error input result
  = Success (InputStream input) result
  | Failure [ErrorMsg error]
  deriving (Eq)

type Position = Int

newtype Parser error input result
  = Parser { runParser' :: InputStream input -> Result error input result }

data InputStream a = InputStream { stream :: a, curPos :: Position }
                   deriving (Show, Eq)

data ErrorMsg e = ErrorMsg { errors :: [e], pos :: Position }
                deriving (Eq)

makeError e p = ErrorMsg [e] p

initPosition = 0

runParser :: Parser error input result -> input -> Result error input result
runParser parser input = runParser' parser (InputStream input initPosition)

toStream :: a -> Position -> InputStream a
toStream = InputStream

incrPos :: InputStream a -> InputStream a
incrPos (InputStream str pos) = InputStream str (pos + 1)

instance Functor (Result error input) where
  fmap f (Success i r) = Success i (f r)
  fmap f (Failure e)   = Failure e

instance Functor (Parser error input) where
  fmap f = Parser . fmap (fmap f) . runParser'

instance Applicative (Parser error input) where
  pure x = Parser $ \s -> Success s x

  Parser p1 <*> Parser p2 = Parser $ \s -> case p1 s of
    Failure e   -> Failure e
    Success i r -> r <$> p2 i

instance Monad (Parser error input) where
  Parser x >>= k = Parser $ \s -> case x s of
    Failure e   -> Failure e
    Success i r -> runParser' (k r) i

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure [makeError mempty (curPos input)]
  Parser a <|> Parser b = Parser $ \input ->
    case a input of
      Success input' r -> Success input' r
      Failure e ->
        case b input of
          Failure e' -> Failure $ mergeErrors e e'
          x          -> x

instance Monoid error => MonadPlus (Parser error input)

instance Monoid error => MonadFail (Parser error input) where
  fail _ = empty

mergeErrors :: (Monoid e) => [ErrorMsg e] -> [ErrorMsg e] -> [ErrorMsg e]
mergeErrors e e' = merge (sortOn pos e) (sortOn pos e')
  where
    merge [] s = s
    merge s [] = s
    merge (ErrorMsg e p : xs) (ErrorMsg e' p' : xs') | p == p' = ErrorMsg (e <> e') p : merge xs xs'
    merge (ErrorMsg e p : xs) e'@(ErrorMsg _ p' : _) | p < p' = ErrorMsg e p : merge xs e'
    merge e@(ErrorMsg _ p : _) (ErrorMsg e' p' : xs) | p > p' = ErrorMsg e' p' : merge xs e

infixl 1 <?>
(<?>) :: Monoid error => error -> Parser error input a -> Parser error input a
(<?>) msg (Parser p) = Parser $ \input ->
  case p input of
    Failure err -> Failure $ mergeErrors [makeError msg (maximum $ map pos err)] err
    x -> x

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: (Show a, Eq a) => a -> Parser String [a] a
symbol c = ("Expected symbol: " ++ show c) <?> satisfy (== c)

eof :: Parser String [a] ()
eof = Parser $ \input ->
  if null $ stream input then
   Success input ()
  else
    Failure [makeError "Not eof" (curPos input)]

notEof :: Parser String [a] ()
notEof = Parser $ \input ->
  if not $ null $ stream input then
    Success input ()
  else
    Failure [makeError "Unexpected eof" (curPos input)]

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \(InputStream input pos) ->
  case input of
    (x:xs) | p x -> Success (incrPos $ InputStream xs pos) x
    input        -> Failure [makeError "Predicate failed" pos]

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' msg = Parser $ \input -> Failure [makeError msg (curPos input)]

word :: (Show a, Eq a) => [a] -> Parser String [a] [a]
word w = Parser $ \(InputStream input pos) ->
  let (pref, suff) = splitAt (length w) input in
  if pref == w
  then Success (InputStream suff (pos + length w)) w
  else Failure [makeError ("Expected " ++ show w) pos]

-- Применяет парсер ко всей последовательности
parseMaybe :: Parser e [t] a -> [t] -> Maybe a
parseMaybe = parse_

parse_ :: Alternative f => Parser e [t] a -> [t] -> f a
parse_ p i = case runParser p i of
  Success (InputStream [] _) x -> pure x
  _                            -> empty

parse :: (Show (ErrorMsg e), MonadError String m) =>
  Parser e [t] a -> [t] -> m a
parse p i = case runParser p i of
  Success (InputStream [] _) x  -> pure x
  Failure es                    -> throwError $ show es
  Success (InputStream _ pos) _ -> throwError $
    "Parser stopped on position " ++ show pos

-- Игнорирует результат парсера
ignore :: Parser e i a -> Parser e i ()
ignore = (() <$)

-- Применяет парсер, при неудаче возвращая значение по умолчанию
option :: Monoid e => a -> Parser e i a -> Parser e i a
option x = (<|> pure x)

-- Версия option_, игнорирующая результат
option_ :: Monoid e => Parser e i a -> Parser e i ()
option_ = option () . ignore

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату,
-- оставляя его во входной последовательности.
scout :: (a -> Bool) -> Parser String [a] a
scout p = Parser $ \ i@(InputStream input pos) ->
  case input of
    (x:xs) | p x -> Success i x
    input        -> Failure [makeError "Predicate failed" pos]

-- Проверяет, что в начале входного потока нет символа, удовлетворяющего предикату
avoid :: (a -> Bool) -> Parser String [a] ()
avoid = (eof <|>) . ignore . scout . fmap not

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy1 sep elem = (:) <$> elem
  <*> many (sep *> elem)

sepBy :: Monoid e => Parser e i sep -> Parser e i a -> Parser e i [a]
sepBy sep elem = sepBy1 sep elem <|> pure []

instance Show (ErrorMsg String) where
  show (ErrorMsg e pos) = "at position " ++ show pos ++ ":\n" ++ (unlines $ map ('\t':) (nub e))

instance (Show input, Show result) => Show (Result String input result) where
  show (Failure e) = "Parsing failed\n" ++ unlines (map show e)
  show (Success i r) = "Parsing succeeded!\nResult:\n" ++ show r ++ "\nSuffix:\t" ++ show i
