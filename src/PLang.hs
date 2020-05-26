{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

module PLang where

import Data.List (intercalate)

type Args = [Arg]
type Body = [Atom]

data Program = Program { relations :: [Relation], goal :: Body }

data Relation = Relation { name :: String
                         , rules :: [Rule] }

data Rule = Rule { params :: Args
                 , body :: Body }

data Arg = AtomArg Atom
         | VarArg String

data Atom = Atom { func :: String, args :: Args }


instance {-# OVERLAPPING #-} Show Args where
  show = intercalate ", " . map show

instance {-# OVERLAPPING #-} Show Body where
  show = (++ ".") . intercalate ", " . map show

instance Show Program where
  show (Program relations goal) =
    intercalate "\n\n" (map show relations)
    ++ "\n\n?- " ++ show goal

instance Show Relation where
  show (Relation name rules) = 
    intercalate "\n" $ map showRule rules
      where
        showRule (Rule params body) = 
          name ++ "(" ++ show params ++ ") :- " ++ show body

instance Show Arg where
  show (AtomArg atom) = show atom
  show (VarArg s)     = s

instance Show Atom where
  show (Atom func args) = func ++ "(" ++ show args ++ ")"
