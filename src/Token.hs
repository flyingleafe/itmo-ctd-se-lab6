{-# LANGUAGE AllowAmbiguousTypes #-}

module Token
       ( ArithToken (..)
       , TokenVisitor (..)
       , charToToken
       , runVisitor
       ) where

data ArithToken = LeftP
                | RightP
                | Plus
                | Minus
                | Mul
                | Div
                | Num Int
                deriving (Show, Eq)

charToToken :: Char -> Maybe ArithToken
charToToken '(' = Just LeftP
charToToken ')' = Just RightP
charToToken '+' = Just Plus
charToToken '-' = Just Minus
charToToken '*' = Just Mul
charToToken '/' = Just Div
charToToken _   = Nothing

class Monad m => TokenVisitor m where
  visit :: ArithToken -> m ()

runVisitor :: TokenVisitor m => [ArithToken] -> m ()
runVisitor toks = mapM_ visit toks
