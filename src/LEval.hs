module LEval where

import LLang (Program (..), Configuration (..))

evalProg :: Program -> [Int] -> Maybe Configuration
evalProg = error "evalProg not implemented"

parseAndEvalProg :: String -> [Int] -> Maybe Configuration
parseAndEvalProg = error "parseAndEvalProg not implemented"