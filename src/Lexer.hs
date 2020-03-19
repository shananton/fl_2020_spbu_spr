module Lexer where

import qualified AST                 as A

import           Combinators
import           Control.Applicative
import           Control.Monad.State
import           Data.Functor
import           Data.Monoid         (Alt (..))
import Data.Char

type EnumStringRepr enum = [(enum, String)]

enum :: (Char -> Bool) -> EnumStringRepr enum -> Parser String String enum
enum inAlphabet repr = getAlt $
  foldMap (Alt . toParser) repr
  where
    toParser (e, s) = e <$ string s <* avoid inAlphabet

type TId = String

data TSep
  = LPar
  | RPar
  | Newline
  | LBrace
  | RBrace
  deriving (Eq, Show)

-- Newline is dealt with separately, because we want to keep track of indentation
sepRepr :: EnumStringRepr TSep
sepRepr = [ (LPar, "("), (RPar, ")"), (LBrace, "{"), (RBrace, "}") ]

sep = enum (const False) sepRepr -- separators can be followed by anything

data TOperator
  = Arith { getArith :: A.Operator }
  | Assign
  deriving (Eq, Show)

operatorRepr :: EnumStringRepr TOperator
operatorRepr
  = [ (Arith A.Plus, "+")
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
    , (Assign, "=")
    ]

operator :: Parser String String TOperator
operator = enum (`elem` alphabet) operatorRepr
  where
    alphabet = "+-*/^&|=<>.$!@:~"

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
keywordRepr
  = [ (KIf, "if")
    , (KThen, "then")
    , (KElse, "else")
    , (KWhile, "while")
    , (KDo, "do")
    , (KRead, "read")
    , (KWrite, "write")
    ]

keyword :: Parser String String TKeyword
keyword = enum inAlphabet keywordRepr
  where
    inAlphabet = (||) <$> isAlphaNum <*> (== '_')

data TIndent
  = Indent
  | Dedent
  deriving (Eq, Show)

data Token
  = TInt { getInt :: Int }
  | TId { getId :: TId }
  | TOperator { getOp :: TOperator }
  | TSep { getSep :: TSep }
  | TKeyword { getKeyword :: TKeyword }
  | TIndent { getIndent :: TIndent }
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
    <|> TSep <$> sep
    <|> TKeyword <$> keyword
    <|> TId <$> ident

type IndentLevels = [Int]

-- Парсеры для разных вкусов цифр

nonzero :: Parser String String Char
nonzero = satisfy ((&&) <$> isDigit <*> (/= '0'))

digit :: Parser String String Char
digit = satisfy isDigit

octal :: Parser String String Char
octal = satisfy (`elem` "01234567")

hex :: Parser String String Char
hex = satisfy isHexDigit

-- Парсер для натуральных чисел
nat :: Parser String String Int
nat = 0 <$ symbol '0' <* avoid isAlphaNum
  <|> (convert 10 <$> ((:) <$> nonzero <*> many digit) <|> convert 8
       <$> (symbol '0' *> some octal) <|> convert 16
       <$> ((string "0x" <|> string "0X") *> some hex)) <* avoid isAlphaNum
  where
    convert b = foldl (\acc x -> b * acc + digitToInt x) 0

-- Парсер для идентификаторов
ident :: Parser String String String
ident = (:) <$> nondigit
  <*> many anychar
  where
    nondigit = satisfy $ (||) <$> isAlpha <*> (== '_')
    anychar  = satisfy $ (||) <$> isAlphaNum <*> (== '_')

wspace = oneOf " \t"

eof = shouldFail elem'

eol = ignore (string "\r\n") <|> ignore (string "\n") <|> eof

lexAll :: Parser String String [Token]
lexAll = evalStateT lexLines [ 0 ]
  where
    dedentTo :: Int -> StateT IndentLevels (Parser String String) [Token]
    dedentTo n = do
      cur <- gets head
      if n < cur then
        modify tail >> (TIndent Dedent :) <$> dedentTo n
      else do
        True <- pure $ n == cur
        return []

    indentTo :: Int -> StateT IndentLevels (Parser String String) [Token]
    indentTo n = do
      cur <- gets head
      if n > cur then
        modify (n :) $> [TIndent Indent]
      else
        dedentTo n

    lexLineWithoutIndentation :: Parser String String [Token]
    lexLineWithoutIndentation = (++)
      <$> some (many wspace *> token)
      <*> (many wspace *> (eol $> [TSep Newline] <|> lineCont))
      where
        lineCont = symbol '\\' *> many wspace *> eol *> many wspace
          *> lexLineWithoutIndentation

    lexLine :: StateT IndentLevels (Parser String String) [Token]
    lexLine = do
      lineIndent <- lift $ length <$> many wspace
      lift ([] <$ eol) <|> -- if the line consists of whitespace, skip it
        (++) <$> indentTo lineIndent <*> lift lexLineWithoutIndentation

    lexLines :: StateT IndentLevels (Parser String String) [Token]
    lexLines = (++)
      <$> (concat <$> many (lift (shouldFail eof) *> lexLine))
      <*> dedentTo 0
