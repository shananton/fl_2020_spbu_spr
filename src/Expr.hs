module Expr where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators
import           Lexer

import           Control.Applicative (many, some, (<|>))
import           Control.Monad       ((>=>))
import           Data.Bifunctor      (first)
import           Data.Char           (digitToInt, isAlpha, isAlphaNum, isDigit,
                                      isHexDigit)
import           Data.Function       ((&))
import           Data.Functor        (($>))
import qualified Data.Map            as Map
import           Data.Maybe          (fromJust, maybe)
import           Data.Monoid         (Alt (..))
import           Data.Tuple          (swap)

data Associativity
  = LeftAssoc  -- 1 @ 2 @ 3 @ 4 = (((1 @ 2) @ 3) @ 4)
  | RightAssoc -- 1 @ 2 @ 3 @ 4 = (1 @ (2 @ (3 @ 4))
  | NoAssoc    -- Может быть только между двумя операндами: 1 @ 2 -- oк; 1 @ 2 @ 3 -- не ок

data OpType = Binary Associativity
            | Unary

evalExpr :: Subst -> AST -> Maybe Int
evalExpr s = eval where

  eval (Num x) = Just x
  eval (Ident v) = Map.lookup v s

  eval (UnaryOp op e) =
    let unOps =
          [ (Plus, id)
          , (Minus, negate)
          , (Not, fromEnum . (== 0))
          ] in
    lookup op unOps <*> eval e

  eval (BinOp op el er) =
    let binOps =
          [ (Plus, (+))
          , (Mult, (*))
          , (Minus, (-))
          , (Div, div)
          , (Pow, (^))
          , (Equal, fromEnum .- (==))
          , (Nequal, fromEnum .- (/=))
          , (Gt, fromEnum .- (>))
          , (Ge, fromEnum .- (>=))
          , (Lt, fromEnum .- (<))
          , (Le, fromEnum .- (<=))
          , (And, boolean (&&))
          , (Or, boolean (||))
          ] in
    lookup op binOps <*> eval el <*> eval er
      where
        boolean f x y = fromEnum $ f (x /= 0) (y /= 0)
        (.-) = (.) . (.)


uberExpr :: Monoid e
         => [(Parser e i op, OpType)] -- список операций с их арностью и, в случае бинарных, ассоциативностью
         -> Parser e i ast            -- парсер элементарного выражения
         -> (op -> ast -> ast -> ast) -- конструктор узла дерева для бинарной операции
         -> (op -> ast -> ast)        -- конструктор узла для унарной операции
         -> Parser e i ast
uberExpr allOps atom astBin astUn = uber allOps
  where
    astr l o r = astBin o l r
    astl o r l = astBin o l r
    uber ((op, opType):ops) = let term = uber ops in
      case opType of
        Binary LeftAssoc  -> foldl (&) <$> term <*> many (astl <$> op <*> term)
        Binary RightAssoc -> flip (foldr ($)) <$> many (astr <$> term <*> op) <*> term
        Binary NoAssoc    -> astr <$> term <*> op <*> term <|> term
        Unary             -> flip (foldr ($)) <$> many (astUn <$> op) <*> term
    uber [] = atom

expr :: Parser String [Token] AST
expr = uberExpr ops atom BinOp UnaryOp
  where
    ops = map (first listToParser)
              [ ([Or], Binary RightAssoc)
              , ([And], Binary RightAssoc)
              , ([Not], Unary)
              , ([Equal, Nequal, Lt, Le, Gt, Ge], Binary NoAssoc)
              , ([Plus, Minus], Binary LeftAssoc)
              , ([Mult, Div], Binary LeftAssoc)
              , ([Plus, Minus], Unary)
              , ([Pow], Binary RightAssoc)
              ]
      where
        listToParser = fmap (getArith . getOperator) . getAlt
          . foldMap (Alt . symbol . TOperator . Arith)
    atom = Num . getInt <$> satisfy isInt
        <|> Ident . getId <$> satisfy isId
        <|> symbol (TOperator LPar) *> expr <* symbol (TOperator RPar)
        <|> funcCall
        where
          funcCall = FunctionCall . getId <$> satisfy isId
             <* symbol (TOperator LPar)
            <*> sepBy (symbol (TOperator Comma)) expr
             <* symbol (TOperator RPar)

-- Парсер для выражений над +, -, *, /, ^ (возведение в степень)
-- с естественными приоритетами и ассоциативностью над натуральными числами с 0.
-- В строке могут быть скобки
parseExpr :: Parser String String AST
parseExpr = Parser
  $ maybe (Failure [makeError "parseExpr failed" 0]) (Success (InputStream "" 0))
  . (parseMaybe lexAll >=> parseMaybe (expr <* symbol (TSep Newline)))
  . stream
