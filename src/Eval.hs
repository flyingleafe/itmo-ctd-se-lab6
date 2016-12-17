{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Eval
       ( Eval
       ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.State  (MonadState (..), StateT (..), execStateT)

import           Common               (peek, pop, push)
import Visitor
import Token

type Eval = StateT [Int] (Either String)

instance TokenVisitor Eval where
  type Result Eval = Int

  visit (Num n) = push n
  visit Plus    = ((+) <$> pop <*> pop) >>= push
  visit Minus   = (flip (-) <$> pop <*> pop) >>= push
  visit Mul     = ((*) <$> pop <*> pop) >>= push
  visit Div     = (flip div <$> pop <*> pop) >>= push

  finish = do
    ls <- get
    case ls of
      [_] -> pure ()
      _     -> throwError "Eval: invalid stack at the end!"

  result = fmap head . flip execStateT []
