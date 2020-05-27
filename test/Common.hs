{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE StandaloneDeriving #-}
module Common where

import GHC.Generics (Generic)
import Control.Exception (try, evaluate, SomeException)
import Test.Tasty.HUnit (assertFailure)
import Control.DeepSeq
import PLang
import LexP
import ParseP

assertFails :: NFData a => a -> IO ()
assertFails action = 
  (try (evaluate $ force action)) >>= \r -> case r of
    Left (_ :: SomeException) -> return ()
    Right _ -> assertFailure "No exception received, but expected exception"

assertSucceeds :: NFData a => a -> IO ()
assertSucceeds action = 
  (try (evaluate $ force action)) >>= \r -> case r of
    Left (_ :: SomeException) -> assertFailure "Exception caught"
    Right _ -> return ()

deriving instance Generic Token
instance NFData Token
deriving instance Generic Program
instance NFData Program
deriving instance Generic Relation
instance NFData Relation
deriving instance Generic Rule
instance NFData Rule
deriving instance Generic Arg
instance NFData Arg
deriving instance Generic Atom
instance NFData Atom
