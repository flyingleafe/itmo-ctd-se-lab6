{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Visitor
       ( TokenVisitor (..)
       , runVisitor
       ) where

import Token

class Monad m => TokenVisitor m where
  type Result m :: *

  visit :: ArithToken -> m ()
  finish :: m ()
  result :: m a -> Either String (Result m)

runVisitor :: forall m. (TokenVisitor m, Show (Result m)) => [ArithToken] -> Either String (Result m)
runVisitor toks = result $ mapM_ visit toks >> finish @m
