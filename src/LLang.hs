module LLang where

import           AST                 (AST (..), Operator (..), Subst (..))
import           Combinators
import           Expr
import           Lexer               hiding (Assign)
import qualified Lexer               as L

import           Control.Applicative
import           Control.Monad       ((>=>))
import           Control.Monad.State
import           Data.Bool           (bool)
import           Data.List           (intercalate, sortOn, find)
import qualified Data.Map            as Map
import           Data.Maybe          (maybe)
import           Data.Function       (on)
import           Text.Printf         (printf)
import           Control.Arrow       ((&&&))

type Expr = AST

type Var = String

data Configuration = Conf { subst :: Subst, input :: [Int], output :: [Int] }
                   deriving (Show, Eq)

data Program = Program { functions :: [Function], main :: LAst }

data Function = Function { name :: String, args :: [Var], funBody :: LAst }

data LAst
  = If { cond :: Expr, thn :: LAst, els :: LAst }
  | While { cond :: AST, body :: LAst }
  | Assign { var :: Var, val :: Expr }
  | Read { var :: Var }
  | Write { val :: Expr }
  | Seq { statements :: [LAst] }
  | Return { val :: Expr }
  deriving (Eq)

-- parseL :: Parser String String LAst
-- parseL = Parser
--   $ maybe (Failure [makeError "parseL failed" 0]) (Success (InputStream "" 0))
--   . (parseMaybe lexAll >=> parseMaybe program)
--   . stream

program :: Parser String [Token] Program
program = do
  functions <- some function
  "Duplicate definitions found" <?>
    guard (isUniqueOn (name &&& args) functions)
  main <- "No main() function found" <?> findMain functions
  return $ Program (filter (not . isMain) functions) main
    where
      findMain functions = do
        Just main <- pure $ funBody <$> find isMain functions
        return main
      isUniqueOn f xs = let sorted = sortOn f xs in
        and $ zipWith ((/=) `on` f) sorted (tail sorted)
      isMain = (("main", []) ==) . (name &&& args)

function :: Parser String [Token] Function
function = functionFull <|> functionShort
  where
    functionSignature = Function <$> identifier
       <* op LPar
      <*> sepBy (op Comma) identifier
       <* op RPar

    functionFull = functionSignature <* kw KIs <* nl <*> block

    functionShort = functionSignature <* op L.Assign
      <*> (Return <$> expr) <* nl


block :: Parser String [Token] LAst
block = sequence <$ sep Indent <*> some stmt <* sep Dedent
  where
    sequence xs = case concatMap toList xs of
      [x] -> x
      xs  -> Seq xs
      where
        toList (Seq ys) = ys
        toList stmt = [stmt]

    stmt = inlineBlock <* nl <|> ifShort <|> ifFull <|> whileShort <|> whileFull

    inlineBlock = sequence <$> sepBy1 (op Comma) inlineStmt
    inlineStmt = inlineAssign
             <|> inlineRead
             <|> inlineWrite
             <|> inlineReturn
             <|> inlineExpr
      where
        inlineAssign = Assign <$> identifier <* op L.Assign <*> expr
        inlineRead = Read <$ kw KRead <*> identifier
        inlineWrite = Write <$ kw KWrite <*> expr
        inlineReturn = Return <$ kw KReturn <*> expr
        inlineExpr = Assign "_" <$> expr

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

-- Common helper parsers
nl = symbol (TSep Newline)
identifier = getId <$> satisfy isId
kw = symbol . TKeyword
sep = symbol . TSep
op = symbol . TOperator

initialConf :: [Int] -> Configuration
initialConf input = Conf Map.empty input []

eval :: LAst -> Configuration -> Maybe Configuration
eval p = execStateT (evalM p) where
  evalM :: LAst -> StateT Configuration Maybe ()
  evalM (If cond thn els) =
    (/= 0) <$> evalExprM cond >>= bool (evalM els) (evalM thn)
  evalM while@(While cond body) =
    (/= 0) <$> evalExprM cond >>= (`when` (evalM body >> evalM while))
  evalM (Assign v e) = do
    x <- evalExprM e
    c@Conf { subst = s } <- get
    put $ c { subst = Map.insert v x s }
  evalM (Read v) = do
    c@Conf { subst = s, input = x : xs } <- get
    put $ c { subst = Map.insert v x s, input = xs }
  evalM (Write e) = do
    c@Conf { output = xs } <- get
    x <- evalExprM e
    put $ c { output = x : xs }
  evalM (Seq s) = foldr ((>>) . evalM) (pure ()) s

  evalExprM :: Expr -> StateT Configuration Maybe Int
  evalExprM e = do
    s <- gets subst
    lift $ evalExpr s e

instance Show Function where
  show (Function name args funBody) =
    printf "%s(%s) =\n%s" name (intercalate ", " $ map show args) (unlines $ map (indentation 1) $ lines $ show funBody)

instance Show Program where
  show (Program defs main) =
    printf "%s\n\n%s" (intercalate "\n\n" $ map show defs) (show main)

instance Show LAst where
  show =
      go 0
    where
      go n t =
        let makeIndent = indentation n in
        case t of
          If cond thn els -> makeIndent $ printf "if %s\n%sthen\n%s\n%selse\n%s" (flatShowExpr cond) (makeIndent "") (go (indent n) thn) (makeIndent "") (go (indent n) els)
          While cond body -> makeIndent $ printf "while %s\n%sdo\n%s" (flatShowExpr cond) (makeIndent "") (go (indent n) body)
          Assign var expr -> makeIndent $ printf "%s := %s" var (flatShowExpr expr)
          Read var        -> makeIndent $ printf "read %s" var
          Write expr      -> makeIndent $ printf "write %s" (flatShowExpr expr)
          Seq stmts       -> intercalate "\n" $ map (go n) stmts
          Return expr     -> makeIndent $ printf "return %s" (flatShowExpr expr)
      flatShowExpr (BinOp op l r) = printf "(%s %s %s)" (flatShowExpr l) (show op) (flatShowExpr r)
      flatShowExpr (UnaryOp op x) = printf "(%s %s)" (show op) (flatShowExpr x)
      flatShowExpr (Ident x) = x
      flatShowExpr (Num n) = show n
      flatShowExpr (FunctionCall name args) = printf "%s(%s)" name (intercalate ", " $ map flatShowExpr args)


      indent = (+1)

indentation n = if n > 0 then printf "%s|_%s" (concat $ replicate (n - 1) "| ") else id
