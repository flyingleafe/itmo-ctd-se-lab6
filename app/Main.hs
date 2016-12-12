module Main where

import           Control.Arrow (Kleisli (..), (&&&))

import           Eval
import           Lexer
import           Parser
import           Print
import           Token

main :: IO ()
main = do
  str <- getContents
  case runLexer str >>= runParser >>= runKleisli (Kleisli runEval &&& Kleisli runPrint) of
    Left err -> putStrLn $ "Error: " ++ err
    Right (res, expr) -> do
      putStrLn $ "Expression in RPN: " ++ expr
      putStrLn $ "Result of the expression: " ++ show res
