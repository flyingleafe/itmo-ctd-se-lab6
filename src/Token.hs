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
                deriving Eq

instance Show ArithToken where
  show LeftP   = "("
  show RightP  = ")"
  show Plus    = "+"
  show Minus   = "-"
  show Mul     = "*"
  show Div     = "/"
  show (Num n) = show n

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
