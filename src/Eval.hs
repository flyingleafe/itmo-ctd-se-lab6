{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Eval
       ( runEval
       ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.State  (MonadState (..), StateT (..), evalStateT)

import           Common               (peek, pop, push)
import           Token

type Eval = StateT [Int] (Either String)

runEval :: [ArithToken] -> Either String Int
runEval toks = evalStateT (runVisitor toks >> checkFinish) []

instance TokenVisitor Eval where
  visit (Num n) = push n
  visit Plus    = ((+) <$> pop <*> pop) >>= push
  visit Minus   = (flip (-) <$> pop <*> pop) >>= push
  visit Mul     = ((*) <$> pop <*> pop) >>= push
  visit Div     = (flip div <$> pop <*> pop) >>= push

checkFinish :: Eval Int
checkFinish = do
  ls <- get
  case ls of
    [res] -> return res
    _     -> throwError "Eval: invalid stack at the end!"
