import ParseP (parse)
import LexP (tokenize)

import System.Environment (getArgs)

main :: IO ()
main = do
  [path] <- getArgs
  text <- readFile path
  let tokens = tokenize text
  let program = parse tokens
  putStrLn $ show program
