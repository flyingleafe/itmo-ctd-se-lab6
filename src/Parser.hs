{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser
       ( Parser
       ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.Loops  (whileM_)
import           Control.Monad.State  (MonadState (..), StateT (..), gets, modify, evalStateT)
import           Control.Monad.Writer (MonadWriter (..), WriterT (..), execWriterT)
import           Data.DList           (DList, toList)

import           Common
import           Token
import           Visitor

type Parser = StateT [ArithToken] (WriterT (DList ArithToken) (Either String))

instance TokenVisitor Parser where
  type Result Parser = [ArithToken]

  visit = processToken
  finish = get >>= mapM_ append
  result = fmap toList . execWriterT . flip evalStateT []

processToken :: ArithToken -> Parser ()
processToken n@(Num _) = append n
processToken Plus      = flushWhile (morePriority Plus) >> push Plus
processToken Minus     = flushWhile (morePriority Minus) >> push Minus
processToken Mul       = flushWhile (morePriority Mul) >> push Mul
processToken Div       = flushWhile (morePriority Div) >> push Div
processToken LeftP     = push LeftP
processToken RightP    = flushWhile (/= LeftP) >> pop >> return ()

flushWhile :: (ArithToken -> Bool) -> Parser ()
flushWhile p = whileM_ (mp <$> peek) $ pop >>= append
  where mp (Just t) = p t
        mp _        = False

priority :: ArithToken -> Int
priority Plus  = 0
priority Minus = 0
priority Mul   = 1
priority Div   = 1
priority _     = 2

morePriority :: ArithToken -> ArithToken -> Bool
morePriority a b = priority a <= priority b
