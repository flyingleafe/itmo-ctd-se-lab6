{-# LANGUAGE FlexibleInstances    #-}
{-# LANGUAGE TypeSynonymInstances #-}

module Parser
       ( runParser
       ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.Loops  (whileM_)
import           Control.Monad.State  (MonadState (..), StateT (..), gets, modify)
import           Control.Monad.Writer (MonadWriter (..), WriterT (..), execWriterT)
import           Data.DList           (DList, toList)

import           Common
import           Token

import           Debug.Trace

type Parser = StateT [ArithToken] (WriterT (DList ArithToken) (Either String))

runParser :: [ArithToken] -> Either String [ArithToken]
runParser toks = fmap toList $ execWriterT $ runStateT (runVisitor toks >> finishParse) []

instance TokenVisitor Parser where
  visit = processToken

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
morePriority a b = priority a >= priority b

finishParse :: Parser ()
finishParse = get >>= mapM_ append
