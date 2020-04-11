{-# LANGUAGE FlexibleInstances #-}

module Combinators where

import           Control.Applicative
import           Data.List           (nub, sortBy)

data Result error input result
  = Success (InputStream input) result
  | Failure [ErrorMsg error]
  deriving (Eq)

type Position = Int

newtype Parser error input result
  = Parser { runParser' :: (InputStream input) -> Result error input result }

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

instance Functor (Parser error input) where
  fmap = error "fmap not implemented"

instance Applicative (Parser error input) where
  pure = error "pure not implemented"
  (<*>) = error "<*> not implemented"

instance Monad (Parser error input) where
  return = error "return not implemented"

  (>>=) = error ">>= not implemented"

instance Monoid error => Alternative (Parser error input) where
  empty = Parser $ \input -> Failure [makeError mempty (curPos input)]

  Parser a <|> Parser b = Parser $ \input ->
    case a input of
      Success input' r -> Success input' r
      Failure e ->
        case b input of
          Failure e' -> Failure $ mergeErrors e e'
          x          -> x

mergeErrors :: (Monoid e) => [ErrorMsg e] -> [ErrorMsg e] -> [ErrorMsg e]
mergeErrors e e' =
    merge (sortBy sorting e) (sortBy sorting e')
  where
    merge [] s = s
    merge s [] = s
    merge (ErrorMsg e p : xs) (ErrorMsg e' p' : xs') | p == p' = ErrorMsg (e <> e') p : merge xs xs'
    merge (ErrorMsg e p : xs) e'@(ErrorMsg _ p' : _) | p < p' = ErrorMsg e p : merge xs e'
    merge e@(ErrorMsg _ p : _) (ErrorMsg e' p' : xs) | p > p' = ErrorMsg e' p' : merge xs e

    sorting x y = pos x `compare` pos y

infixl 1 <?>
(<?>) :: Monoid error => error -> Parser error input a -> Parser error input a
(<?>) msg (Parser p) = Parser $ \input ->
    case p input of
      Failure err -> Failure $ mergeErrors [makeError msg (maximum $ map pos err)] err
      x -> x

-- Проверяет, что первый элемент входной последовательности -- данный символ
symbol :: Char -> Parser String String Char
symbol c = ("Expected symbol: " ++ show c) <?> satisfy (== c)

eof :: Parser String String ()
eof = Parser $ \input -> if null $ stream input then Success input () else Failure [makeError "Not eof" (curPos input)]

-- Проверяет, что первый элемент входной последовательности удовлетворяет предикату
satisfy :: (a -> Bool) -> Parser String [a] a
satisfy p = Parser $ \(InputStream input pos) ->
  case input of
    (x:xs) | p x -> Success (incrPos $ InputStream xs pos) x
    input        -> Failure [makeError "Predicate failed" pos]

-- Успешно парсит пустую строку
epsilon :: Parser e i ()
epsilon = success ()

-- Всегда завершается успехом, вход не читает, возвращает данное значение
success :: a -> Parser e i a
success a = Parser $ \input -> Success input a

-- Всегда завершается ошибкой
fail' :: e -> Parser e i a
fail' msg = Parser $ \input -> Failure [makeError msg (curPos input)]

word :: String -> Parser String String String
word w = Parser $ \(InputStream input pos) ->
  let (pref, suff) = splitAt (length w) input in
  if pref == w
  then Success (InputStream suff (pos + length w)) w
  else Failure [makeError ("Expected " ++ show w) pos]

instance Show (ErrorMsg String) where
  show (ErrorMsg e pos) = "at position " ++ show pos ++ ":\n" ++ (unlines $ map ('\t':) (nub e))

instance (Show input, Show result) => Show (Result String input result) where
  show (Failure e) = "Parsing failed\n" ++ unlines (map show e)
  show (Success i r) = "Parsing succeeded!\nResult:\n" ++ show r ++ "\nSuffix:\t" ++ show i
