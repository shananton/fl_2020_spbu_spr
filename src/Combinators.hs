{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-} -- извините
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}

module Combinators where

import           Control.Applicative
import           Control.Monad
import           Control.Monad.Fail
import           Data.List           (nub, sortOn)
import           Control.Monad.Except
import Control.Lens
import Control.Lens.TH

data InputStream a = InputStream { stream :: a, curPos :: Position }
                   deriving (Show, Eq)

data ErrorMsg e = ErrorMsg { errors :: [e], pos :: Position }
                deriving (Eq)

data Result error input result
  = Success input result
  | Failure [ErrorMsg error]
  deriving (Eq)

data Position = Pos { line :: Int, col :: Int }
  deriving (Show, Eq, Ord)

newtype Parser error input result
  = Parser { runParser :: input -> Result error input result }

makeError e p = ErrorMsg [e] p

initPosition = Pos 0 0

-- runParser :: Parser error input result -> input -> Result error input result
-- runParser parser input = runParser' parser (InputStream input initPosition)

toStream :: a -> Position -> InputStream a
toStream = InputStream

instance Functor (Result error input) where
  fmap f (Success i r) = Success i (f r)
  fmap f (Failure e)   = Failure e

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

instance (Stream input a, Monoid error) => Alternative (Parser error input) where
  empty = Parser $ \s -> Failure [makeError mempty (locate s)]
  Parser a <|> Parser b = Parser $ \input ->
    case a input of
      Success input' r -> Success input' r
      Failure e ->
        case b input of
          Failure e' -> Failure $ mergeErrors e e'
          x          -> x

instance (Stream input a, Monoid error) => MonadPlus (Parser error input)

instance (Stream input a, Monoid error) => MonadFail (Parser error input) where
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

class Stream s t | s -> t where
  next :: s -> Maybe (t, s)
  locate :: s -> Position

instance Stream (InputStream String) Char where
  next (InputStream [] _)                  = Nothing
  next (InputStream (x:xs) (Pos line col)) = let
    npos = case x of
      '\t' -> Pos line (col + defaultTabWidth)
      '\r' -> Pos line col
      '\n' -> Pos (line + 1) 0
      _    -> Pos line (col + 1)
    in Just (x, InputStream xs npos)
  locate = curPos

defaultTabWidth = 4

position :: Stream s t => Parser e s Position
position = Parser $ \ input -> Success input (locate input)

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: (Stream s t, Show t, Eq t) => t -> Parser String s t
symbol c = ("Expected symbol: " ++ show c) <?> satisfy (== c)

eof :: Stream s t => Parser String s ()
eof = "Not eof" <?> shouldFail anyChar

notEof :: Stream s t => Parser String s ()
notEof = "Unexpected eof" <?> void peek

shouldFail :: Stream s t => Parser String s a -> Parser String s ()
shouldFail p = Parser $ \ s -> case runParser p s of
  Success _ _ -> Failure [makeError "Parser in shouldFail succeeded" (locate s)]
  Failure _   -> Success s ()

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: Stream s t => (t -> Bool) -> Parser String s t
satisfy p = Parser $ \ s ->
  case next s of
    Nothing            -> Failure [makeError "Unexpected eof" (locate s)]
    Just (x, s') | p x -> Success s' x
    _                  -> Failure [makeError "Predicate failed" (locate s)]

anyChar :: Stream s t => Parser String s t
anyChar = satisfy (const True)

-- Всегда завершается ошибкой
fail' :: Stream s t => e -> Parser e s t
fail' msg = Parser $ \s -> Failure [makeError msg (locate s)]

word :: (Stream s t, Show t, Eq t) => [t] -> Parser String s [t]
word w = do
  let err = "Expected " ++ show w
  word <- err <?> replicateM (length w) anyChar
  err <?> guard (word == w)
  return word

-- Применяет парсер ко всей последовательности
parseMaybe :: Stream s t => Parser String s a -> s -> Maybe a
parseMaybe = parse_

parse_ :: (Stream s t, Alternative f) => Parser String s a -> s -> f a
parse_ p i = case runParser (p <* eof) i of
  Success _ x -> pure x
  _           -> empty


parse :: (Stream s t, MonadError String m) =>
  Parser String s a -> s -> m a
parse p i = case runParser (p <* eof) i of
  Success _ x  -> pure x
  Failure es   -> throwError $ show es

-- Применяет парсер, при неудаче возвращая значение по умолчанию
option :: (Stream s t, Monoid e) => a -> Parser e s a -> Parser e s a
option x = (<|> pure x)

-- Версия option_, игнорирующая результат
option_ :: (Stream s t, Monoid e) => Parser e s a -> Parser e s ()
option_ = option () . void

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату,
-- оставляя его во входной последовательности.
scout :: Stream s t => (t -> Bool) -> Parser String s t
scout p = Parser $ \ s ->
  case next s of
    Nothing           -> Failure [makeError "Unexpected eof" (locate s)]
    Just (x, _) | p x -> Success s x
    _                 -> Failure [makeError "Predicate failed" (locate s)]

peek :: Stream s t => Parser String s t
peek = scout (const True)

-- Проверяет, что в начале входного потока нет символа, удовлетворяющего предикату
avoid :: Stream s t => (t -> Bool) -> Parser String s ()
avoid = shouldFail . scout

-- Принимает последовательность элементов, разделенных разделителем
-- Первый аргумент -- парсер для разделителя
-- Второй аргумент -- парсер для элемента
-- В последовательности должен быть хотя бы один элемент
sepBy1 :: (Stream s t, Monoid e) => Parser e s sep -> Parser e s a -> Parser e s [a]
sepBy1 sep elem = (:) <$> elem
  <*> many (sep *> elem)

sepBy :: (Stream s t, Monoid e) => Parser e s sep -> Parser e s a -> Parser e s [a]
sepBy sep elem = sepBy1 sep elem <|> pure []

instance Show (ErrorMsg String) where
  show (ErrorMsg e pos) = "at position " ++ show pos ++ ":\n" ++ (unlines $ map ('\t':) (nub e))

instance (Show input, Show result) => Show (Result String input result) where
  show (Failure e) = "Parsing failed\n" ++ unlines (map show e)
  show (Success i r) = "Parsing succeeded!\nResult:\n" ++ show r ++ "\nSuffix:\t" ++ show i
