{-# LANGUAGE FlexibleContexts #-}

module Common
       ( peek
       , pop
       , push
       , append
       ) where

import           Control.Monad.Except (MonadError, throwError)
import           Control.Monad.State  (MonadState, gets, modify)
import           Control.Monad.Writer (MonadWriter (..))
import           Data.DList           (DList, singleton)
import           Safe                 (headMay)

peek :: MonadState [a] m => m (Maybe a)
peek = gets headMay

pop :: (MonadError String m, MonadState [a] m) => m a
pop = (gets headMay >>=
       maybe (throwError "Stack is empty: invalid input sequence!") pure)
      <* modify tail

push :: MonadState [a] m => a -> m ()
push = modify . (:)

append :: MonadWriter (DList a) m => a -> m ()
append = tell . singleton
