module Test.ParseP where

import Control.Monad ((>=>))
import Test.Tasty.HUnit (Assertion, (@?=))
import Common (assertFails)

import LexP (tokenize)
import ParseP
import PLang

lexAndParse = parse . tokenize

assertParses :: String -> Assertion
assertParses s = show (lexAndParse s) @?= s

assertFileParses :: FilePath -> Assertion
assertFileParses = readFile >=> (assertParses . init)

unit_simpleParse :: Assertion
unit_simpleParse = do
  lexAndParse "?-." @?= Program
    { relations = []
    , goal = []
    }
  lexAndParse "a(X) :- true. ?-." @?= Program
    { relations = 
      [ Relation
        { name = "a"
        , rules = [Rule [VarArg "X"] [Atom "true" []]]
        }
      ]
    , goal = []
    }

unit_samples :: Assertion
unit_samples = do
  assertFileParses "test/samples/sample.p"
  assertFileParses "test/samples/lists.p"
