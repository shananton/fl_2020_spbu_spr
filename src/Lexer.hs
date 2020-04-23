{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE FlexibleContexts #-}

module Lexer where

import qualified AST                 as A

import           Combinators
import           Control.Applicative
import           Control.Monad.State
import           Data.Functor
import           Data.Monoid         (Alt (..))
import Data.Char (isDigit, isHexDigit, digitToInt)
import Data.Ord (Down (..))
import Data.List (sortOn)
import Control.Monad.Except
import Control.Lens hiding (enum)
import Control.Lens.TH

-- Наборы символов

isAlpha :: Char -> Bool
isAlpha x = 'a' <= x && x <= 'z' || 'A' <= x && x <= 'Z'

isAlnum :: Char -> Bool
isAlnum x = isAlpha x || isDigit x || x == '_'

isSpace = (`elem` " \t")

space :: Parser String String Char
space = satisfy isSpace

alpha :: Parser String String Char
alpha = satisfy isAlpha

alnum :: Parser String String Char
alnum = satisfy isAlnum

digit :: Parser String String Char
digit = satisfy isDigit

hex :: Parser String String Char
hex = satisfy isHexDigit

-- Общий вид для перечислений

type EnumStringRepr enum = [(enum, String)]

normalize :: EnumStringRepr a -> EnumStringRepr a
normalize = sortOn (Down . length . snd)

enum :: EnumStringRepr enum -> Parser String String enum
enum repr = getAlt $ foldMap (Alt . toParser) repr where
  toParser (e, s) = e <$ word s

-- Виды токенов

type TId = String

data TSep = Newline
          | Indent
          | Dedent
  deriving (Eq, Show)

data TOperator = Arith { getArith :: A.Operator }
               | Assign
               | LPar
               | RPar
               | Comma
  deriving (Eq, Show)

operatorRepr :: EnumStringRepr TOperator
operatorRepr = normalize
  [ (Arith A.Plus, "+")
  , (Arith A.Mult, "*")
  , (Arith A.Minus, "-")
  , (Arith A.Div, "/")
  , (Arith A.Pow, "^")
  , (Arith A.Equal, "==")
  , (Arith A.Nequal, "/=")
  , (Arith A.Gt, ">")
  , (Arith A.Ge, ">=")
  , (Arith A.Lt, "<")
  , (Arith A.Le, "<=")
  , (Arith A.And, "&&")
  , (Arith A.Or, "||")
  , (Arith A.Not, "!")
  , (Assign, "=")
  , (LPar, "(")
  , (RPar, ")")
  , (Comma, ",")
  ]

operator :: Parser String String TOperator
operator = enum operatorRepr

data TKeyword
  = KIf
  | KThen
  | KElse
  | KWhile
  | KDo
  | KRead
  | KWrite
  | KPass
  | KReturn
  | KIs
  deriving (Eq, Show)

keywordRepr :: EnumStringRepr TKeyword
keywordRepr = normalize
  [ (KIf, "if")
  , (KThen, "then")
  , (KElse, "else")
  , (KWhile, "while")
  , (KDo, "do")
  , (KRead, "read")
  , (KWrite, "write")
  , (KPass, "pass")
  , (KReturn, "return")
  , (KIs, "is")
  ]

keyword :: Parser String String TKeyword
keyword = enum keywordRepr <* avoid isAlnum

data Token
  = TInt { getInt_ :: Int }
  | TId { getId_ :: TId }
  | TOperator { getOperator_ :: TOperator }
  | TSep { getSep_ :: TSep }
  | TKeyword { getKeyword_ :: TKeyword }
  deriving (Eq, Show)

data FixedPosStream a = FixedPosStream [(a, Position)]

instance Stream (FixedPosStream a) a where
  next (FixedPosStream (x, p):s') = Just (x, s')
  next _                          = Nothing
  locate

isInt :: Token -> Bool
isInt (TInt _) = True
isInt _        = False

isId :: Token -> Bool
isId (TId _) = True
isId _       = False

token :: Parser String String Token
token = TInt <$> nat
    <|> TOperator <$> operator
    <|> TKeyword <$> keyword
    <|> TId <$> ident

parseRaw :: (Show (ErrorMsg e), MonadError String m) =>
  Parser e [Token] a -> String -> m a
parseRaw p = parse lexAll >=> parse p

parseRawEither :: (Show (ErrorMsg e)) =>
  Parser e [Token] a -> String -> Either String a
parseRawEither = parseRaw

-- Парсер для натуральных чисел
nat :: Parser String String Int
nat = nat' <* avoid isAlnum
    where
      nat' = convert 16 <$ (word "0x" <|> word "0X") <*> some hex
         <|> convert 10 <$> some digit
      convert b = foldl (\acc x -> b * acc + digitToInt x) 0

-- Парсер для идентификаторов
ident :: Parser String String String
ident = (:) <$> alpha <*> many alnum

eol = void (word "\r\n") <|> void (word "\n") <|> eof

type IndentLevels = [Int]

lexAll :: Parser String String [TokenP]
lexAll = evalStateT lexLines [0]
  where
    dedentTo :: Int -> StateT IndentLevels (Parser String String) [TokenP]
    dedentTo n = do
      cur <- gets head
      if n < cur then
        modify tail >>
          (:) <$> lift (pinned (TSep Dedent)) <*> dedentTo n
      else do
        True <- pure $ n == cur
        return []

    indentTo :: Int -> StateT IndentLevels (Parser String String) [TokenP]
    indentTo n = do
      cur <- gets head
      if n > cur then
        modify (n :) >>
          (: []) <$> lift (pinned (TSep Indent))
      else
        dedentTo n

    lexLineWithoutIndentation :: Parser String String [TokenP]
    lexLineWithoutIndentation = (++)
      <$> some (many space *> emit token)
      <*> (many space *>
        (eol >> (: []) <$> pinned (TSep Newline)) <|> lineCont)
      where
        lineCont = symbol '\\' *> many space *> eol *> many space
          *> lexLineWithoutIndentation

    lexLine :: StateT IndentLevels (Parser String String) [TokenP]
    lexLine = do
      lineIndent <- lift $ length <$> many space
      lift ([] <$ eol) <|> -- if the line consists of whitespace, skip it
        (++) <$> indentTo lineIndent <*> lift lexLineWithoutIndentation

    lexLines :: StateT IndentLevels (Parser String String) [TokenP]
    lexLines = (++)
      <$> (concat <$> many (lift notEof *> lexLine))
      <*> dedentTo 0
