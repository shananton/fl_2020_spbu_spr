module LLang where

import AST (AST (..), Operator (..), Subst (..))
import Combinators
import Expr
import Lexer hiding (Assign)
import qualified Lexer as L

import           Control.Monad       ((>=>))
import Control.Applicative
import Data.Maybe (maybe)
import qualified Data.Map as Map

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, val :: Expr }
  | Read { var :: Var }
  | Write { val :: Expr }
  | Seq { statements :: [LAst] }
  deriving (Show, Eq)

parseL :: Parser String String LAst
parseL = Parser
  $ maybe (Failure "parseL failed") (Success "")
  . (parseMaybe lexAll >=> parseMaybe program)

program :: Parser String [Token] LAst
program = sequence <$> many stmt
  where
    nl = symbol (TSep Newline)
    name = getId <$> satisfy isId
    kw = symbol . TKeyword
    sep = symbol . TSep
    op = symbol . TOperator
    sequence [x] = x
    sequence xs  = Seq xs

    stmt = inlineBlock <* nl <|> ifShort <|> ifFull <|> whileShort <|> whileFull

    inlineBlock = sequence <$> sepBy1 (op Comma) inlineStmt
    inlineStmt = inlineAssign <|> inlineRead <|> inlineWrite where
      inlineAssign = Assign <$> name <* op L.Assign <*> expr
      inlineRead = Read <$ kw KRead <*> name
      inlineWrite = Write <$ kw KWrite <*> expr

    block = sequence <$ sep Indent <*> some stmt <* sep Dedent

    ifShort = If <$ kw KIf <*> expr <* kw KThen
      <*> inlineBlock
      <*> option (Seq []) (kw KElse *> inlineBlock)
      <* nl

    ifFull = If <$ kw KIf <*> expr <* kw KThen <* nl
      <*> block
      <*> option (Seq []) (kw KElse *> nl *> block)

    whileShort = While <$ kw KWhile <*> expr <* kw KDo
      <*> inlineBlock
      <* nl

    whileFull = While <$ kw KWhile <*> expr <* kw KDo <* nl
      <*> block

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval = error "eval not defined"
