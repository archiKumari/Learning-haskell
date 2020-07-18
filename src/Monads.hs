module Monads where

import Data.Char
import Data.Maybe
import Control.Monad.Reader
import Control.Monad

data Person = Per {
  pname :: String,
  age :: Int
  }
  deriving (Show, Eq)

v :: Person
v = Per "hari" 23

gg :: ReaderT Person Maybe String
gg = do
 --p <- ask
 pa <- asks age
 --lift $ Just "as"
 let str = if pa < 25 then "Young" else "Old"
 return (str)

data Student = St {
  name :: String,
  roll :: Int,
  marks :: Int
  }
  deriving Show

stRec :: [Student]
stRec = [St "Ajay" 1 325,St "Ritu" 2 297,St "Priya" 3 214]

disp :: ReaderT Student Maybe Student
disp = do
 stats <- ask
 rec <- ask >>= \x -> pure $ marks x
 let percent = div (100*rec) 400
     pos = if percent > 75 then "First Position" else if percent > 65 then "Second Position" else "Third Position"
 return stats

modSt :: Student -> Student
modSt (St name roll marks) = St name' roll marks'
  where name' = map toUpper name
        marks' = marks+10

dispName :: ReaderT Student Maybe String 
dispName = do
 name' <- asks name
 return (name')

localFn :: ReaderT Student Maybe Student
localFn = do
 stData <- ask
 lift $ runReaderT (local modSt disp) $ stData

strFn :: ReaderT String IO ()
strFn = do
 str <- ask
 lift $ print "before local"
 lift $ print str
 local ((const '@') <$>) (ff) -- >>= \_-> hh) >>= (\x-> pure $ x*2) >>= lift. print
 str1 <- ask
 lift $ print "after local"
 lift $ print str1

ff :: ReaderT String IO String
ff = do
  env <- ask
  lift $ print "inside local"
  lift $ print env
  return (env <> env )

hh :: ReaderT String IO Int
hh = do
  env <- ask
  return (length env)
