{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Print
       ( runPrint
       ) where

import           Control.Monad.Writer (MonadWriter (..), Writer (..), execWriter)

import           Token

type Print = Writer String

instance TokenVisitor Print where
  visit tok = tell $ show tok ++ " "

runPrint :: [ArithToken] -> Either String String
runPrint = Right . execWriter . runVisitor
