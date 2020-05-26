{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PLang where

import Data.List (intercalate)

type Args = [Arg]
type Body = [Atom]

data Relation = Relation { name :: String
                         , arity :: Int
                         , rules :: [Rule] }

data Rule = Rule { params :: Args
                 , body :: Body }

data Arg = AtomArg Atom
         | VarArg String

data Atom = Atom { func :: String, args :: Args }

newtype Goal = Goal { goal :: Body }


instance {-# OVERLAPPING #-} Show Args where
  show = intercalate ", " . map show

instance {-# OVERLAPPING #-} Show Body where
  show = (++ ".") . intercalate ", " . map show

instance Show Relation where
  show (Relation name _ rules) = 
    intercalate "\n" $ map showRule rules
      where
        showRule (Rule params body) = 
          name ++ "(" ++ show params ++ ") :- " ++ show body

instance Show Arg where
  show (AtomArg atom) = show atom
  show (VarArg s)     = s

instance Show Atom where
  show (Atom func args) = func ++ "(" ++ show args ++ ")"

instance Show Goal where
  show (Goal goal) = "?- " ++ show goal
