module Print
       ( Print
       ) where

import           Control.Monad.Writer (MonadWriter (..), Writer (..), execWriter)

import Visitor

type Print = Writer String

instance TokenVisitor Print where
  type Result Print = String

  visit tok = tell $ show tok ++ " "
  finish = pure ()
  result = Right . execWriter
