module Arith where

import Text.Printf (printf)
import Text.Read (readMaybe)
import Data.Char (isDigit, digitToInt)
import Data.Tuple (swap)
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Control.Applicative
import Control.Monad
import Control.Monad.Fail
import Data.Monoid

import qualified Sum (parseNum, splitOn)

-- "1+2*3+4*2" -> 15
data Operator = Plus
              | Mult
              | Minus
              | Div
              deriving (Eq)

operators :: [(String, Operator)]
operators = [("+", Plus), ("*", Mult), ("-", Minus), ("/", Div)]

instance Show Operator where
    show = fromJust . (`lookup` map swap operators)

data AST = BinOp Operator AST AST
         | Num  Int
         deriving (Eq)

-- Преобразует дерево в строку
-- Между числами и знаками операций по одному пробелу
-- BinOp Plus (Num 13) (Num 42) -> "13 42 +"
toPostfix :: AST -> String
toPostfix (BinOp op l r) = intercalate " " $
    [toPostfix l, toPostfix r, show op]
toPostfix (Num x)        = show x

-- Парсит выражение в постфиксной записи
-- Выражение принимается только целиком (не максимально длинный префикс)
-- Между числами и знаками операций по одному пробелу
-- "13 42 +" -> Just (BinOp Plus (Num 13) (Num 42))
-- "1 2 3 +" -> Nothing
-- "1 2 + *" -> Nothing
fromPostfix :: String -> Maybe AST
fromPostfix input = do
    [res] <- foldM f [] $ Sum.splitOn ' ' input
    return res
    where
        f stack tok = (: stack) . Num <$> readMaybe tok <|> do
            op <- lookup tok operators
            r:l:rest <- Just stack
            return $ BinOp op l r : rest

newtype Parser a = Parser {runParser :: String -> Maybe (a, String)}

instance Functor Parser where
    fmap f = Parser . (fmap $ fmap $ swap . fmap f . swap) . runParser

instance Applicative Parser where
    pure x = Parser $ \s -> Just (x, s)
    Parser p1 <*> Parser p2 = Parser $ \s -> do
        (f, t) <- p1 s
        (x, t') <- p2 t
        return (f x, t')

instance Alternative Parser where
    empty = Parser $ const Nothing
    Parser p1 <|> Parser p2 = Parser $ \s -> p1 s <|> p2 s

instance Monad Parser where
    Parser p >>= k = Parser $ \s -> do
        (x, t) <- p s
        (y, t') <- runParser (k x) t
        return (y, t')

instance MonadPlus Parser

instance MonadFail Parser where
    fail _ = empty

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = Parser $ \s -> do
    c:t <- Just s
    guard $ p c
    return (c, t)

char :: Char -> Parser Char
char c = satisfy (== c)

num :: Parser AST
num = Num . Sum.parseNum <$> some (satisfy isDigit)

operator :: Parser Operator
operator = Parser parseOp

opMult :: Parser Operator
opMult = do
    op <- operator
    guard (op == Mult || op == Div)
    return op

opSum :: Parser Operator
opSum = do
    op <- operator
    guard (op == Plus || op == Minus)
    return op

endoConcat :: [a -> a] -> a -> a
endoConcat = appEndo . mconcat . map Endo

multExpr :: Parser AST
multExpr = endoConcat <$> many (term <**> (BinOp <$> opMult)) <*> term

sumExpr :: Parser AST
sumExpr = endoConcat <$> many (multExpr <**> (BinOp <$> opSum)) <*> multExpr

term :: Parser AST
term = num <|> char '(' *> sumExpr <* char ')'

-- Парсит левую скобку
parseLbr :: String -> Maybe ((), String)
parseLbr = runParser $ () <$ char '('

-- Парсит правую скобку
parseRbr :: String -> Maybe ((), String)
parseRbr = runParser $ () <$ char ')'

parseExpr :: String -> Maybe (AST, String)
parseExpr = parseSum

parseNum :: String -> Maybe (AST, String)
parseNum = runParser num

parseOp :: String -> Maybe (Operator, String)
parseOp ('+':xs) = Just (Plus, xs)
parseOp ('*':xs) = Just (Mult, xs)
parseOp ('-':xs) = Just (Minus, xs)
parseOp ('/':xs) = Just (Div, xs)
parseOp _ = Nothing

parseMult :: String -> Maybe (AST, String)
parseMult = runParser multExpr

parseSum :: String -> Maybe (AST, String)
parseSum = runParser sumExpr

evaluate :: String -> Maybe Int
evaluate input = do
    (ast, rest) <- parseExpr input
    return $ compute ast

compute :: AST -> Int
compute (Num x) = x
compute (BinOp Plus x y) = compute x + compute y
compute (BinOp Mult x y) = compute x * compute y
compute (BinOp Minus x y) = compute x - compute y
compute (BinOp Div x y) = compute x `div` compute y

instance Show AST where
  show  = printf "\n%s" . go 0
    where
      go n t =
        (if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id) $
        case t of
          BinOp op l r -> printf "%s\n%s\n%s" (show op) (go (ident n) l) (go (ident n) r)
          Num i -> show i
      ident = (+1)
