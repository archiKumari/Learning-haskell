module WriterMonad where

import Control.Monad.Writer

fn :: WriterT String IO ()
fn = do
  tell "tell"
  (_,w) <- listen func
  --liftIO $ print w
  writer (1," writer") >>= \x -> writer (2*x,"written")
  --liftIO $ print m
  (_,r) <- listen func
  --liftIO $ print r
  return ()

func :: WriterT String IO Int
func = do
  writer (1, "Added")
  return 5
