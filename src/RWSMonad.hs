module RWSMonad where

import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer
import Control.Monad.RWS

data Command = Plus | Minus
  deriving (Show,Eq)

rwsLoop :: WriterT [Command] (ReaderT String (StateT Int IO)) ()
rwsLoop = do
  env <- ask
  count <- get
  input <- lift $ lift $ lift getLine
  case input of
    "p" -> do
      modify (succ)
      tell [Plus]
      cc <- get
      liftIO $ print ("Add" ++ show cc)
      rwsLoop
    "m" -> do
      modify (pred)
      tell [Minus]
      cc <- get
      liftIO $ print ("Minus" ++ show cc)
      rwsLoop
    {-"e" -> do
      liftIO $ putStrLn "End"
      liftIO $ print ("State: " ++ show count)
      liftIO $ print ("Env: " ++ env)-}
    _   -> do
      liftIO $ print "End"


ff :: RWST String [Command] Int IO ()
ff = do
  env <- ask
  count <- get
  input <- lift getLine
  case input of
    "p" -> do
      modify (succ)
      tell [Plus]
      cc <- get
      liftIO $ print ("Add" ++ show cc)
      ff
    "m" -> do
      modify (pred)
      tell [Minus]
      cc <- get
      liftIO $ print ("Minus" ++ show cc)
      ff
    _   -> do
      liftIO $ print "End"
