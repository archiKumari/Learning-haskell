module StateMonad where

import System.Random
import Control.Monad.State
import Control.Monad.Reader

fn :: StateT String IO String
fn = do
  inp <- get
  put ("Put")
  modify (++ " Modify")
  state (\s -> (s,s++"State"))
  -- return (inp)

func :: StateT String IO String
func = do
  state (\s -> (s,s++"State"))

counterLoop :: StateT Int IO ()
counterLoop = do
  count <- get
  lift $ print count
  input <- lift getLine
  case input of
    "+" -> do 
      put (count+1)
      counterLoop
    "-" -> do
      put (count-1)
      counterLoop
    "e" -> lift $ putStrLn ("End")
    _   -> do
      lift $ print "Error"
      counterLoop

rsLoop :: ReaderT String (StateT Int IO) ()
rsLoop = do
  env <- ask
  count <- get
  input <- lift $ lift getLine
  case input of
    "+" -> do
      modify (+1)
      rsLoop
    "-" -> do
      put (count-1)
      rsLoop
    "e" -> do
      liftIO $ putStrLn "End"
      liftIO $ print ("State: " ++ show count)
      liftIO $ print ("Env: " ++ env)
    _   -> lift.lift $ print "Error"
