{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE LiberalTypeSynonyms #-}
module LEval where

import LLang (Program (..), Configuration (..), Defs (..), LAst (..), Expr (..), FunctionSignature (..), Function (..), signature, program)
import AST
import Expr (evalExpr)
import Lexer (parseRawEither)

import Control.Applicative ((<|>), Alternative (..))
import Control.Monad.RWS
import Control.Monad.State
import Control.Monad.Except
import Control.Lens
import Control.Lens.TH
import           Control.Arrow ((&&&))
import Data.Map ((!), fromList)
import qualified Data.Map as Map
import Data.Maybe (maybe)
import Data.Bool (bool)

data ImmutableState = ImmutableState { _funDefs :: Defs, _capturedVars :: Subst }
type Output = Dual [Int]
newtype Input = Input { _istream :: [Int] }

newtype LocalState = LocalState { _localVars :: Subst }

$(makeLenses ''ImmutableState)
$(makeLenses ''Input)
$(makeLenses ''LocalState)

type Returning a = ExceptT a
type ProgramM = RWST ImmutableState Output Input Maybe
type FunctionM = Returning Int (StateT LocalState ProgramM)

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg (Program fs main) inp = do
  let defs = makeDefs fs
  let code = runExceptT (evalM main)
  let fun = views localVars Conf <$> execStateT code (LocalState Map.empty)
  (partialConf, Input inp', Dual out) <- runRWST
    fun (ImmutableState defs Map.empty) (Input inp)
  return $ partialConf inp' out defs

  where
    makeDefs = fromList . map (signature &&& id)

    evalF :: LAst -> ProgramM (Maybe Int)
    evalF p = let
      fun = do
        Right ret <- runExceptT ((Nothing <$ evalM p) `catchError` (return . Just))
        return ret
      in evalStateT fun $ LocalState Map.empty

    evalM :: LAst -> FunctionM ()
    evalM (If cond thn els) =
      (/= 0) <$> evalExprM cond >>= bool (evalM els) (evalM thn)
    evalM while@(While cond body) =
      (/= 0) <$> evalExprM cond >>= (`when` (evalM body >> evalM while))
    evalM (Assign v e) =
      evalExprM e >>= assignVar v
    evalM (Read v) = do
      x : _ <- lift $ lift $ istream <<%= drop 1
      assignVar v x
    evalM (Write e) =
      evalExprM e >>= write
    evalM (Seq s) =
      foldr ((>>) . evalM) (pure ()) s
    evalM (Return e) =
      evalExprM e >>= throwError

    write :: Int -> FunctionM ()
    write x = tell $ Dual [x]

    assignVar :: String -> Int -> FunctionM ()
    assignVar v x = localVars . at v ?= x

    evalExprM :: Expr -> FunctionM Int
    evalExprM (Num x) = pure x
    evalExprM (Ident v) = getValue v
    evalExprM (UnaryOp op e) =
      let unOps =
            [ (Plus, id)
            , (Minus, negate)
            , (Not, fromEnum . (== 0))
            ] in
      liftMaybe (lookup op unOps) <*> evalExprM e
    evalExprM (BinOp op el er) =
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
      liftMaybe (lookup op binOps) <*> evalExprM el <*> evalExprM er
        where
          boolean f x y = fromEnum $ f (x /= 0) (y /= 0)
          (.-) = (.) . (.)
    evalExprM (FunctionCall name vals) = do
      Just (Function _ args body)  <- view (funDefs . at (Sig name (length vals)))
      argsMapping <- fromList . zip args <$> mapM evalExprM vals
      lift $ lift $ do
        Just ret <- local (capturedVars .~ argsMapping) (evalF body)
        return ret

    liftMaybe :: Maybe a -> FunctionM a
    liftMaybe = lift . lift . lift

    getValue :: String -> FunctionM Int
    getValue v = do
      Just x <- (<|>)
        <$> use (localVars . at v)
        <*> view (capturedVars . at v)
      return x

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg text inp = do
  Right p <- pure $ parseRawEither program text
  evalProg p inp
