module ReaderMonad where

import Control.Monad
import Control.Monad.Reader
import Data.Map.Strict
import qualified Data.Map.Strict as M

type Env = Map SymName Value
type SymName = String
type Value   = String

-- | Find a value related to a symbol if present in the 
-- env in Reader Monadic Context
findVal :: SymName -> ReaderT Env IO (Maybe Value)
findVal sym = do
  env <- ask
  return $ M.lookup sym env

data Command =
    Assign String String
  | Lookup String
  | Exit
  | InvalidCommand
  deriving (Show)

parseCommand :: String -> Command
parseCommand str 
 | elem '=' str = Assign symName value
 | str == "exit" = Exit
 | length str == 1 = Lookup str
 | otherwise = InvalidCommand
  where symName = takeWhile (/='=') str
        value = tail $ dropWhile (/='=') str

readerloop :: ReaderT Env IO String
readerloop = do
  env <- ask
  input <- lift getLine
  case parseCommand input of
    Assign name value -> do
      lift $ print ("New Value Added")
      local (insert name value) readerloop
    Lookup symName -> case M.lookup symName env of
      Nothing -> do
        lift $ print ("The value for " ++ symName ++ " is not defined!")
        readerloop
      Just value -> do
        lift $ print (value)
        readerloop
    Exit -> return (show env)
    InvalidCommand -> do
      lift $ print "Invalid Command!"
      readerloop
