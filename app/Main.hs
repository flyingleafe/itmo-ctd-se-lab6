{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies     #-}

module Main where

import           Common
import           Eval
import           Lexer
import           Parser
import           Print
import           Token
import           Visitor

main :: IO ()
main = do
  putStrLn "Enter the expression and hit Ctrl-D"
  str <- getContents
  let eres = runLexer str >>=
             runVisitor @Parser >>=
             (runVisitor @Eval) &&&& (runVisitor @Print)
  case eres of
    Left err -> putStrLn $ "Error: " ++ err
    Right (res, expr) -> do
      putStrLn $ "Expression in RPN: " ++ expr
      putStrLn $ "Result of the expression: " ++ show res
