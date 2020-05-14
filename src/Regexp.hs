module Regexp where

import Prelude hiding (seq)

data Regexp = Empty
            | Epsilon
            | Char Char
            | Seq Regexp Regexp
            | Alt Regexp Regexp
            | Star Regexp
            deriving (Eq, Ord)

match :: Regexp -> String -> Bool
match r s = nullable (foldl (flip derivative) r s)

derivative :: Char -> Regexp -> Regexp
derivative _ Empty     = Empty
derivative _ Epsilon   = Empty
derivative c (Char c') | c == c'   = Epsilon
                       | otherwise = Empty
derivative c (Seq l r) =
  let tryLeft = if nullable l then Alt (derivative c r) else id
    in tryLeft $ Seq (derivative c l) r
derivative c (Alt l r) = Alt (derivative c l) (derivative c r)
derivative c (Star e)  = Seq (derivative c e) (Star e)

nullable :: Regexp -> Bool
nullable Empty = False
nullable Epsilon = True
nullable (Char _) = False
nullable (Seq l r) = nullable l && nullable r
nullable (Alt l r) = nullable l || nullable r
nullable (Star _) = True
