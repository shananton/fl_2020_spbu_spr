module LLang where

import           AST                 (AST (..), Operator (..))
import           Combinators
import           Control.Applicative
import           Data.Functor
import           Expr hiding (expr)
import qualified Expr                as E
import           Lexer hiding (Assign)
import qualified Lexer               as L

type Expr = AST

type Var = String

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, expr :: Expr }
  | Read { var :: Var }
  | Write { expr :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

indent = symbol (TIndent Indent)

dedent = symbol (TIndent Dedent)

name = getId <$> satisfy isId

nl = symbol (TSep Newline)

block :: Parser String [Token] LAst
block = indent $> Seq <*> some (stmtFull <* nl) <* dedent

stmtFull :: Parser String [Token] LAst
stmtFull = ifStmtFull <|> whileStmtFull <|> readStmt <|> writeStmt <|> assignStmt
 where
  ifStmtFull = symbol (TKeyword KIf) $>  If
      <*> E.expr
      <*  symbol (TKeyword KThen) <* nl
      <*> block
      <*> option (Seq [])
        (symbol (TKeyword KElse) *> nl *> block)
  whileStmtFull = symbol (TKeyword KWhile) $> While
      <*> E.expr
      <*  symbol (TKeyword KDo) <* nl
      <*> block
  assignStmt =
    Assign <$> name <* symbol (TOperator L.Assign) <*> E.expr <* nl
  readStmt  = Read <$ symbol (TKeyword KRead) <*> name <* nl
  writeStmt = Write <$ symbol (TKeyword KWrite) <*> E.expr <* nl

stmt1 :: LAst
stmt1 = Seq
  [ Read "X"
  , If
    (BinOp Gt (Ident "X") (Num 13))
    (Write (Ident "X"))
    (While
      (BinOp Lt (Ident "X") (Num 42))
      (Seq [Assign "X" (BinOp Mult (Ident "X") (Num 7)), Write (Ident "X")])
    )
  ]

