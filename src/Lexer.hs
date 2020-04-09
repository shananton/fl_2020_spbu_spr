module Lexer where

import qualified AST                 as A

import           Combinators
import           Control.Applicative
import           Control.Monad.State
import           Data.Char           (digitToInt, isDigit, isHexDigit)
import           Data.Functor
import           Data.List           (sortOn)
import           Data.Monoid         (Alt (..))
import           Data.Ord            (Down (..))

-- Наборы символов

isAlpha :: Char -> Bool
isAlpha x = 'a' <= x && x <= 'z' || 'A' <= x && x <= 'Z' || x == '_'

isAlnum :: Char -> Bool
isAlnum = (||) <$> isAlpha <*> isDigit

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
  toParser (e, s) = e <$ string s

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
  ]

keyword :: Parser String String TKeyword
keyword = enum keywordRepr <* avoid isAlnum

data Token
  = TInt { getInt :: Int }
  | TId { getId :: TId }
  | TOperator { getOperator :: TOperator }
  | TSep { getSep :: TSep }
  | TKeyword { getKeyword :: TKeyword }
  deriving (Eq, Show)

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


-- Парсер для натуральных чисел
nat :: Parser String String Int
nat = nat' <* avoid isAlnum
    where
      nat' = convert 16 <$ (string "0x" <|> string "0X") <*> some hex
         <|> convert 10 <$> some digit
      convert b = foldl (\acc x -> b * acc + digitToInt x) 0

-- Парсер для идентификаторов
ident :: Parser String String String
ident = (:) <$> alpha <*> many alnum

eof = shouldFail elem'

eol = ignore (string "\r\n") <|> ignore (string "\n") <|> eof

type IndentLevels = [Int]

lexAll :: Parser String String [Token]
lexAll = evalStateT lexLines [0]
  where
    dedentTo :: Int -> StateT IndentLevels (Parser String String) [Token]
    dedentTo n = do
      cur <- gets head
      if n < cur then
        modify tail >> (TSep Dedent :) <$> dedentTo n
      else do
        True <- pure $ n == cur
        return []

    indentTo :: Int -> StateT IndentLevels (Parser String String) [Token]
    indentTo n = do
      cur <- gets head
      if n > cur then
        modify (n :) $> [TSep Indent]
      else
        dedentTo n

    lexLineWithoutIndentation :: Parser String String [Token]
    lexLineWithoutIndentation = (++)
      <$> some (many space *> token)
      <*> (many space *> (eol $> [TSep Newline] <|> lineCont))
      where
        lineCont = symbol '\\' *> many space *> eol *> many space
          *> lexLineWithoutIndentation

    lexLine :: StateT IndentLevels (Parser String String) [Token]
    lexLine = do
      lineIndent <- lift $ length <$> many space
      lift ([] <$ eol) <|> -- if the line consists of whitespace, skip it
        (++) <$> indentTo lineIndent <*> lift lexLineWithoutIndentation

    lexLines :: StateT IndentLevels (Parser String String) [Token]
    lexLines = (++)
      <$> (concat <$> many (lift (shouldFail eof) *> lexLine))
      <*> dedentTo 0
