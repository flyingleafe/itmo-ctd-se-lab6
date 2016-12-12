{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies     #-}
{-# LANGUAGE ViewPatterns     #-}

module Lexer
       ( runLexer
       ) where

import           Control.Monad.Except (throwError)
import           Control.Monad.State  (MonadState (..), StateT (..), evalStateT)
import           Control.Monad.Writer (MonadWriter (..), WriterT (..), execWriterT)
import           Data.Char            (digitToInt, isDigit, isSpace)
import           Data.DList           (DList, singleton, toList)
import           Text.Megaparsec      (Stream (..))

import           Common               (append)
import           Token

data LexerState = Start
                | Number Int

type Lexer = StateT LexerState (WriterT (DList ArithToken) (Either String))

runLexer :: (Stream s, Token s ~ Char) => s -> Either String [ArithToken]
runLexer s = fmap toList $ execWriterT $ evalStateT (lexer s) Start

lexer :: (Stream s, Token s ~ Char) => s -> Lexer ()
lexer s = case uncons s of
  Nothing     -> checkForNumber
  Just (c, s) -> processChar c >> lexer s

checkForNumber :: Lexer ()
checkForNumber = do
  st <- get
  case st of
    Number num -> append $ Num num
    Start      -> return ()

processChar :: Char -> Lexer ()
processChar c
  | isSpace c = return ()
  | otherwise = do
      st <- get
      case st of
        Start ->
          if isDigit c
          then put $ Number $ digitToInt c
          else trySingleToken c
        Number num ->
          if isDigit c
          then put $ Number $ 10 * num + digitToInt c
          else do
            append $ Num num
            trySingleToken c
            put Start

trySingleToken :: Char -> Lexer ()
trySingleToken (charToToken -> Just tok) = append tok
trySingleToken c                         = throwError $ "Unexpected char: " ++ show c
