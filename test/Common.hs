{-# LANGUAGE ScopedTypeVariables #-}
module Common where

import Control.Exception (try, evaluate, SomeException)
import Test.Tasty.HUnit (assertFailure)

assertFails :: a -> IO ()
assertFails action = 
  (try (evaluate action)) >>= \r -> case r of
    Left (_ :: SomeException) -> return ()
    Right _ -> assertFailure "No exception received, but expected exception"
