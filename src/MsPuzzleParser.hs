module MsPuzzleParser where

import Control.Monad
import Control.Monad.Except

import Data.Char

newtype DD = DD Int
  deriving (Show,Eq)

data OP = Plus | Minus
  deriving (Show,Eq)

data Exp = Exp DD OP DD DD
  deriving (Show,Eq)

parseDigit :: MonadError e m => e -> String -> m (DD,String)
parseDigit err str = case takeWhile isDigit str of
  []  -> throwError err
  numStr -> pure (DD num,remList)
    where num = read numStr::Int
          remList = dropWhile (isDigit) str

parseOp :: MonadError e m => e -> String -> m (OP,String)
parseOp err str = case head str of
  '+' -> pure (Plus,(drop 1 str))
  '-' -> pure (Minus,(drop 1 str))
  _   -> throwError err

parseEq :: MonadError e m => e -> String -> m ((), String)
parseEq err str = case head str of
  '=' -> pure ((),(drop 1 str))
  _   -> throwError err

parseExp :: String -> Either String Exp
parseExp input = do
  (d1,rest)  <- parseDigit "Invalid Input" input
  (op,rest1) <- parseOp "Invalid Input" rest
  (d2,rest2) <- parseDigit "Invalid Input" rest1
  (_, rest3) <- parseEq "Invalid Input" rest2
  (d3,rest4) <- parseDigit "Invalid Input" rest3
  return (Exp d1 op d2 d3)
